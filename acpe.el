;;; acpe.el --- Agent Client Protocol for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Daniel Pettersson <daniel@dpettersson.net>
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2026
;; License: GPL-3.0-or-later
;; Version: 0.1.0
;; Homepage: https://github.com/svaante/acpe
;; Package-Requires: ((emacs "29.1") (jsonrpc "1.0.25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Acpe is an Agent Client Protocol (ACP) client for Emacs.
;;
;; Usage:
;;   Call M-x acpe your-favorite-acp-server to start a ACP session.
;;   Use `acpe-prompt' to send a message to the agent.
;;

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'diff-mode)
(require 'eieio)
(require 'jsonrpc)
(require 'project)
(require 'pulse)
(require 'shr)
(require 'url-util)


;;; Customization

(defgroup acpe nil
  "Agent Client Protocol for Emacs."
  :prefix "acpe-"
  :group 'tools)

(defvar acpe-command-history nil
  "History list for ACP agent commands.")

(defcustom acpe-session-store-file
  (locate-user-emacs-file "acpe-sessions.eld")
  "File where acpe stores known session IDs for later resume."
  :type 'file)

(defcustom acpe-markdown-command "pandoc --from markdown --to html"
  "Shell command to convert markdown to HTML.  Nil for raw text."
  :type '(choice string (const nil)))

(defcustom acpe-prompt-finished-hook nil
  "Hook run when the agent finishes responding and awaits input."
  :type 'hook)

(defcustom acpe-permission-requested-hook nil
  "Hook run when the agent requests permission to perform action."
  :type 'hook)


;;; Connection

(defvar acpe--connections nil
  "List of active ACP connections.")

(defvar-local acpe--connection nil
  "Buffer-local `acpe-connection' for prompt and control buffers.")

(defclass acpe-connection (jsonrpc-process-connection)
  ((agent-name
    :accessor acpe--agent-name :initarg :agent-name :initform "unknown"
    :documentation "Name of the agent.")
   (capabilities
    :accessor acpe--capabilities :initform nil
    :documentation "Agent capabilities from initialize response.")
   (session-id
    :accessor acpe--session-id :initform nil
    :documentation "Current session identifier.")
   (initialized-p
    :accessor acpe--initialized-p :initform nil
    :documentation "Non-nil after successful initialization.")
   (working-directory
    :accessor acpe--working-directory :initarg :working-directory
    :initform nil
    :documentation "Project root used as session working directory.")
   (slash-commands
    :accessor acpe--slash-commands :initform nil
    :documentation "Slash commands provided by the agent.")
   (output-buffer
    :accessor acpe--output-buffer :initform nil
    :documentation "Buffer for displaying agent interaction.")
   (cancelling
    :accessor acpe--cancelling :initform nil
    :documentation "Non-nil while awaiting for cancel notification.")
   (pending-permissions
    :accessor acpe--pending-permissions :initform nil
    :documentation "List of deferred permission requests awaiting user response.")
   (config-options
    :accessor acpe--config-options :initform nil
    :documentation "Config options provided by the agent (list of PLISTs).")
   (first-prompt
    :accessor acpe--first-prompt :initform nil
    :documentation "First prompt text sent in this session."))
  :documentation "ACP connection to a coding agent.")


;;; Transport

(cl-defmethod initialize-instance :after ((conn acpe-connection) _slots)
  "Install newline-framed process filter on CONN."
  (when-let* ((proc (ignore-errors (jsonrpc--process conn))))
    (set-process-filter proc #'acpe--process-filter)))

(defun acpe--process-filter (proc string)
  "Newline-framed JSON process filter; parse STRING arriving on PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((conn (process-get proc 'jsonrpc-connection))
            (inhibit-read-only t)
            messages)
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        ;; Extract complete lines and parse each as a JSON message.
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let* ((end (point))
                 (line (buffer-substring-no-properties (point-min)
                                                       (1- end))))
            (delete-region (point-min) end)
            (goto-char (point-min))
            (unless (string-blank-p line)
              (condition-case err
                  (let ((msg (json-parse-string
                              line
                              :object-type 'plist
                              :null-object nil
                              :false-object :json-false)))
                    (push (plist-put msg :jsonrpc-json line) messages))
                (error
                 (message "[acpe] JSON parse error: %s" (cdr err)))))))
        (set-marker (process-mark proc) (point-max))
        (dolist (msg (nreverse messages))
          (run-at-time 0 nil (lambda (msg)
                               (with-temp-buffer
                                 (jsonrpc-connection-receive conn msg)))
                       msg))))))

(cl-defmethod jsonrpc-connection-send ((conn acpe-connection)
                                       &rest args
                                       &key id method _params
                                       (result nil result-supplied-p)
                                       error _partial)
  "Send ARGS to CONN using newline-delimited framing."
  (cl-block jsonrpc-connection-send
    ;; Intercept deferred permission replies: store them for the user to
    ;; respond to via `acpe-accept' or `acpe-cancel'.
    (when (and result-supplied-p (plist-get result :acpe-deferred))
      (setf (acpe--pending-permissions conn)
            (append (acpe--pending-permissions conn)
                    (list (list :id id :method method :deferred result))))
      (acpe--update-permission-queue-positions conn)
      (cl-return-from jsonrpc-connection-send))
    (when method
      (setq args
            (plist-put
             args :method
             (cond ((keywordp method) (substring (symbol-name method) 1))
                   ((symbolp method) (symbol-name method))
                   (t method)))))
    (let* ((kind (cond ((or result-supplied-p error) 'reply)
                       (id 'request)
                       (t 'notification)))
           (converted (jsonrpc-convert-to-endpoint conn args kind))
           (json (json-serialize converted
                                 :null-object nil
                                 :false-object :json-false)))
      (process-send-string (jsonrpc--process conn) (concat json "\n"))
      (jsonrpc--event conn 'client
                      :json json
                      :kind kind
                      :message args
                      :foreign-message converted))))


;;; Request and notification dispatch

(cl-defgeneric acpe-handle-request (_conn method &rest _params)
  "Handle request METHOD from CONN.  Return result or signal `jsonrpc-error'."
  (jsonrpc-error "Method not found: %s" method))

(cl-defgeneric acpe-handle-notification (_conn _method &rest _params)
  "Handle notification METHOD from CONN."
  nil)


;;; Buffer content

(defun acpe--buffer-text (&optional start end)
  "Return buffer text, or lines START to END (1-based)."
  (if (and start end)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (forward-line (- end (1- start)))
          (buffer-substring-no-properties beg (point))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun acpe--file-text (path &optional start end)
  "Return text of PATH from live buffer or disk, lines START to END if given."
  (if-let* ((buffer (find-buffer-visiting path)))
      (with-current-buffer buffer (acpe--buffer-text start end))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (acpe--buffer-text start end)))))

(defun acpe--insert-file (path &optional start end)
  "Return an ACP resource block for PATH, lines START to END (1-based)."
  (when-let* ((text (acpe--file-text path start end)))
    `( :type "resource"
       :resource ( :uri ,(concat "file://" path
                                 (when start
                                   (format "#L%d:%d" start end)))
                   :text ,text
                   :mimeType "text/plain"))))

(defun acpe--insert-buffer (&optional buffer start end)
  "Return an ACP resource block for BUFFER (default current), lines START to END."
  (with-current-buffer (or buffer (current-buffer))
    `( :type "resource"
       :resource ( :uri ,(concat "emacs://buffer/"
                                 (url-hexify-string (buffer-name))
                                 (when start
                                   (format "#L%d:%d" start end)))
                   :text ,(acpe--buffer-text start end)
                   :mimeType "text/plain"))))


;;; Permission request

(cl-defmethod acpe-handle-request (conn (_method (eql session/request_permission))
                                        &key sessionId options toolCall &allow-other-keys)
  "Handle permission request OPTIONS on CONN for SESSIONID."
  (unless (equal sessionId (acpe--session-id conn))
    (jsonrpc-error "Unknown session: %s" sessionId))
  (if (acpe--cancelling conn)
      '(:outcome (:outcome "cancelled"))
    (when-let* ((content (plist-get toolCall :content)))
      (cl-loop for block across content do
               (when-let* ((diff (acpe--extract-diff block)))
                 (acpe--show-diff conn diff))
               (when-let* (((equal (plist-get toolCall :kind) "switch_mode"))
                           (plan (acpe--extract-plan block)))
                 (acpe--show-plan conn plan))))
    ;; Return a sentinel plist; jsonrpc-connection-send will defer
    ;; this reply.  Always store :toolCallId so the permission queue
    ;; can do a live buffer lookup later (session/update may not have
    ;; rendered the tool call yet when this request arrives).
    (run-hooks 'acpe-permission-requested-hook)
    `( :acpe-deferred t
       :options ,(append options nil)
       :toolCallId ,(plist-get toolCall :toolCallId))))


;;; Prompt vars

(defvar-local acpe--output-end nil
  "Marker before the input prompt; insertion-type t advances it past inserts.")

(defvar acpe--prompt-overlay-arrow nil
  "`acpe-prompt-mode' marker for `overlay-arrow-variable-list'.")


;;; Inbound notifications

(defun acpe--update-permission-queue-positions (conn)
  "Move the overlay arrow to the first pending permission on CONN, or hide it."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (force-mode-line-update) ; put or remove ? from mode-line
      (if-let* ((first (car (acpe--pending-permissions conn))))
          (when-let* ((id (plist-get (plist-get first :deferred) :toolCallId)))
            ;; Find our tool call line
            (save-excursion
              (goto-char (point-min))
              (when-let* ((match (text-property-search-forward
                                  'acpe-tool-call-id id #'equal))
                          (tool-data (get-text-property (prop-match-beginning match)
                                                        'acpe-tool-call-data)))
                (goto-char (1+ (prop-match-beginning match)))
                (move-marker acpe--prompt-overlay-arrow (line-beginning-position))
                (pulse-momentary-highlight-one-line)
                (message "Permission requested for %s %s"
                         (plist-get tool-data :title)
                         (substitute-command-keys
                          "\\<acpe-control-mode-map>(\
`\\[acpe-accept]' to accept, \
`\\[acpe-cancel]' to cancel)")))))
        (when (markerp acpe--prompt-overlay-arrow)
          (set-marker acpe--prompt-overlay-arrow nil))))))

(defun acpe--output-end-get (conn prop)
  "Return text property PROP at the last output position in CONN's buffer."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (get-text-property (1- acpe--output-end) prop))))

(cl-defmethod acpe-handle-notification (conn (_method (eql session/update))
                                             &key sessionId update &allow-other-keys)
  "Handle session/update UPDATE from CONN for SESSIONID."
  (when (equal sessionId (acpe--session-id conn))
    (pcase (plist-get update :sessionUpdate)
      ("available_commands_update"
       (setf (acpe--slash-commands conn)
             (append (plist-get update :availableCommands) nil)))
      ("config_option_update"
       (setf (acpe--config-options conn)
             (append (plist-get update :configOptions) nil))
       (acpe--prompt-update-mode-line conn))
      ("agent_thought_chunk"
       (when-let* ((content (plist-get update :content))
                   ((equal (plist-get content :type) "text"))
                   (text (plist-get content :text)))
         (let* ((pending-p (acpe--output-end-get conn 'acpe-thought-pending))
                (invisible (cond ((not pending-p) 'acpe-detail)
                                 ((acpe--output-end-get conn 'invisible)))))
           (unless pending-p
             (acpe--prompt-insert conn "\n" (acpe--render-thought-line))
             (setq text (concat "\n" text)))
           (acpe--prompt-insert
            conn (propertize text
                             'font-lock-face 'shadow
                             'acpe-thought-pending t
                             'acpe-detail t
                             'acpe-details t
                             'invisible invisible)))))
      ("agent_message_chunk"
       (acpe--flush-thought-chunks conn)
       (when-let* ((content (plist-get update :content))
                   ((equal (plist-get content :type) "text"))
                   (text (plist-get content :text)))
         (unless (acpe--output-end-get conn 'acpe-message-chunk)
           (setq text (concat (propertize "\n" 'acpe-message-stream-start t) text)))
         (acpe--prompt-insert conn (propertize text 'acpe-message-chunk t))))
      ("tool_call"
       (acpe--flush-thought-chunks conn)
       (acpe--flush-message-chunks conn)
       (thread-last `( :toolCallId ,(plist-get update :toolCallId)
                       :status ,(or (plist-get update :status) "pending")
                       :title ,(or (plist-get update :title) "tool")
                       :kind ,(plist-get update :kind)
                       :rawInput ,(plist-get update :rawInput))
                    (acpe--render-tool-call-line)
                    (concat "\n")
                    (acpe--prompt-insert conn))
       ;; Retry permission positioning now that tool_call is rendered.
       (acpe--update-permission-queue-positions conn))
      ("tool_call_update"
       (when-let* ((id (plist-get update :toolCallId)))
         (apply #'acpe--update-tool-call conn id update)))
      ("plan"
       (acpe--flush-thought-chunks conn)
       (acpe--flush-message-chunks conn)
       ;; TODO Figure out what to do on plan
       )))
  nil)

;;; Output rendering

(defun acpe--render-markdown (text)
  "Render TEXT as markdown, returning a propertized string."
  (if (not acpe-markdown-command)
      text
    (condition-case _
        (with-temp-buffer
          (insert text)
          (let ((args (split-string-and-unquote acpe-markdown-command)))
            (apply #'call-process-region
                   (point-min) (point-max)
                   (car args) t t nil (cdr args)))
          (shr-render-region (point-min) (point-max))
          (goto-char (point-min))
          ;; Move 'face properties to 'font-lock-face
          (while (< (point) (point-max))
            (let* ((pos (point))
                   (next (or (next-single-property-change pos 'face)
                             (point-max))))
              (when-let* ((face (get-text-property pos 'face)))
                (put-text-property pos next 'font-lock-face face))
              (goto-char next)))
          (buffer-string))
      (error text))))

(defun acpe--flush-message-chunks (conn)
  "Flush and render pending message chunks for CONN."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when-let* ((start (text-property-any (point-min) acpe--output-end
                                            'acpe-message-stream-start t)))
        (let* ((text (buffer-substring-no-properties (1+ start) acpe--output-end))
               (inhibit-read-only t))
          (delete-region start acpe--output-end)
          (acpe--prompt-insert conn "\n" (acpe--render-markdown text)))))))


;;; Thought and Tool lines

(defvar-keymap acpe--toggle-detail-keymap
  :doc "Keymap on foldable tool-call and thought lines."
  "<return>"  #'acpe-toggle-detail-at-point
  "<mouse-1>" #'acpe-toggle-detail-at-point)

(defvar acpe-tool-call-kind-formatters
  `((other . ,(lambda (input)
                (format " %s" (prin1-to-string input))))
    (plan  . ,(lambda (input)
                (format "\n%s" (acpe--render-markdown (plist-get input :plan))))))
  "Alist mapping tool-call kind to a formatter function (rawInput to string).")

(defun acpe--render-tool-call-line (data &optional expanded)
  "Render tool call DATA to a propertized string;
If EXPANDED is non-nil details is hidden."
  (pcase-let* ((id (plist-get data :toolCallId))
               (status (or (plist-get data :status) "pending"))
               (title (or (plist-get data :title) "tool"))
               (`(,face ,label)
                (pcase status
                  ("in_progress" '(compilation-mode-line-run "busy"))
                  ("completed"   '(compilation-info          "done"))
                  ("failed"      '(compilation-error         "fail"))
                  (_             '(shadow                    "pend")))))
    (propertize
     (concat
      "    "
      (propertize
       (concat (propertize (format "[%s]" label) 'font-lock-face face)
               " "
               title)
       'keymap acpe--toggle-detail-keymap
       'mouse-face 'highlight
       'help-echo "TAB or click to toggle output")
      (when-let* ((input (plist-get data :rawInput))
                  (fmt (alist-get (intern (plist-get data :kind))
                                  acpe-tool-call-kind-formatters nil nil #'equal)))
        (propertize (funcall fmt input) 'font-lock-face 'shadow))
      (when-let* ((output (plist-get data :rawOutput)))
        (propertize (concat "\n" (format "%s" output))
                    'font-lock-face 'shadow
                    'invisible (unless expanded 'acpe-detail)
                    'acpe-detail t)))
     'acpe-tool-call-id id
     'acpe-tool-call-data data
     'acpe-details t)))

(defun acpe--update-tool-call (conn id &rest updates)
  "Update tool call ID in CONN's buffer with UPDATES."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when-let* ((match (text-property-search-forward
                            'acpe-tool-call-id id #'equal))
                    (start (prop-match-beginning match))
                    (end (prop-match-end match))
                    (data (append updates
                                  (get-text-property
                                   start 'acpe-tool-call-data))))
          (let ((expanded (acpe--detail-expanded-p start end))
                (inhibit-read-only t))
            (delete-region start end)
            (insert (acpe--render-tool-call-line data expanded))))))))

(defun acpe--detail-expanded-p (start end)
  "Non-nil if acpe-detail is visible in START..END."
  (when-let* ((pos (text-property-any start end 'acpe-detail t)))
    (null (get-text-property pos 'invisible))))

(defun acpe--render-thought-line (&optional details expanded-p)
  "Render a thought summary, with DETAILS with visibility EXPANDED-P."
  (let* ((in-progress (null details))
         (face (if in-progress
                   'compilation-mode-line-run
                 'font-lock-comment-face))
         (label (if in-progress "thinking" "thought"))
         (summary (propertize (format "(%s)" label)
                              'font-lock-face face
                              'acpe-thought-pending in-progress
                              'mouse-face 'highlight
                              'help-echo "TAB or click to toggle"
                              'keymap acpe--toggle-detail-keymap)))
    (propertize
     (concat summary
             (when-let* ((details (acpe--render-markdown details)))
               (propertize (concat "\n" details)
                           'acpe-detail t
                           'font-lock-face 'shadow
                           'invisible (unless expanded-p 'acpe-detail))))
     'acpe-details t)))

(defun acpe--flush-thought-chunks (conn)
  "Flush and finalize the streaming thought in CONN's buffer."
  (when-let* ((conn conn)
              (buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (save-excursion
        (when (and (> acpe--output-end (point-min))
                   (get-text-property (1- acpe--output-end)
                                      'acpe-thought-pending))
          (let* ((start (previous-single-property-change
                         acpe--output-end 'acpe-thought-pending))
                 (detail-pos (text-property-any
                              start acpe--output-end 'acpe-detail t))
                 (text-start (if detail-pos
                                 (1+ detail-pos)
                               acpe--output-end))
                 (details (buffer-substring-no-properties
                           text-start acpe--output-end))
                 (expanded (acpe--detail-expanded-p start acpe--output-end))
                 (inhibit-read-only t))
            (goto-char start)
            (delete-region start acpe--output-end)
            (insert (acpe--render-thought-line details expanded))))))))

(defun acpe-toggle-detail-at-point ()
  "Toggle the detail block for the thought or tool call at point."
  (interactive)
  (if-let* (((get-text-property (point) 'acpe-details))
            (pos (point))
            (start (previous-single-property-change (1+ pos) 'acpe-details))
            (end (next-single-property-change pos 'acpe-details)))
      (when-let* ((detail-pos (text-property-any start end 'acpe-detail t))
                  (detail-end (next-single-property-change detail-pos 'acpe-detail nil end)))
        (put-text-property detail-pos detail-end 'invisible
                           (if (acpe--detail-expanded-p start end)
                               'acpe-detail
                             nil)))
    (user-error "No foldable item at point")))


;;; Review buffers

(defun acpe--extract-diff (content-block)
  "Return a diff plist from CONTENT-BLOCK."
  (when (equal (plist-get content-block :type) "diff")
    (or (plist-get content-block :diff)
        content-block)))

(defun acpe--extract-plan (content-block)
  "Return markdown text from CONTENT-BLOCK."
  (when (equal (plist-get content-block :type) "content")
    (when-let* ((inner (plist-get content-block :content))
                ((equal (plist-get inner :type) "text")))
      (plist-get inner :text))))

(defun acpe--diff-string (old-text new-text path)
  "Return a unified diff string changing OLD-TEXT to NEW-TEXT for PATH."
  (let ((old-file (make-temp-file "acpe-diff-old-"))
        (new-file (make-temp-file "acpe-diff-new-")))
    (unwind-protect
        (progn
          (write-region old-text nil old-file nil 'silent)
          (write-region new-text nil new-file nil 'silent)
          (with-temp-buffer
            (call-process diff-command nil t nil "-u"
                          "--label" (concat "a/" path)
                          "--label" (concat "b/" path)
                          old-file new-file)
            (buffer-string)))
      (delete-file old-file t)
      (delete-file new-file t))))

(defun acpe--show-diff (conn diff)
  "Show DIFF from CONN in a `diff-mode' review buffer."
  (let* ((path (plist-get diff :path))
         (old-text (concat (or (plist-get diff :oldText) "") "\n"))
         (new-text (concat (plist-get diff :newText) "\n"))
         (name (format "*acpe diff: %s*"
                       (file-name-nondirectory path)))
         (unified (acpe--diff-string old-text new-text path))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert unified))
      (diff-mode)
      (setq-local acpe--connection conn
                  default-directory (acpe--working-directory conn))
      (acpe-control-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun acpe--show-plan (conn text)
  "Show TEXT as a rendered plan buffer for CONN."
  (let* ((name (format "*acpe plan: %s*"
                       (file-name-nondirectory
                        (directory-file-name
                         (acpe--working-directory conn)))))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (acpe--render-markdown text)))
      (special-mode)
      (setq-local acpe--connection conn
                  default-directory (acpe--working-directory conn))
      (acpe-control-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))


;;; Session

(defun acpe--request (conn method params &optional callback)
  "Send a request to agent CONN with METHOD and PARAMS.
If CALLBACK is given, call it with (result error).  Otherwise block."
  (if callback
      (jsonrpc-async-request
       conn method params
       :success-fn (lambda (r) (funcall callback r nil))
       :error-fn (lambda (e)
                   (funcall callback nil
                            (plist-get e :message)))
       :timeout nil
       :timeout-fn (lambda ()
                     (funcall callback nil "request timeout")))
    (jsonrpc-request conn method params :timeout nil)))

(defun acpe--initialize (conn)
  "Send initialize to CONN and record capabilities."
  (let* ((result (acpe--request
                  conn 'initialize
                  `( :protocolVersion 1
                     :clientInfo ( :name "acpe"
                                   :title "Agent Client Protocol for Emacs"
                                   :version "0.1.0"))))
         (agent-version (plist-get result :protocolVersion)))
    (unless (equal agent-version 1)
      (error "[acpe] Protocol version mismatch (client=%d agent=%d)"
             1 agent-version))
    (setf (acpe--capabilities conn) (plist-get result :agentCapabilities))
    (setf (acpe--initialized-p conn) t)
    result))

(defun acpe--session-store-read ()
  "Return the stored session registry."
  (when (file-exists-p acpe-session-store-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents acpe-session-store-file)
          (read (current-buffer)))
      (error
       (message "[acpe] Could not read session store %s: %s"
                acpe-session-store-file
                (error-message-string err))
       nil))))

(defun acpe--session-store-write (records)
  "Write session RECORDS to `acpe-session-store-file'."
  (make-directory (file-name-directory acpe-session-store-file) t)
  (with-temp-file acpe-session-store-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 records (current-buffer))
      (insert "\n"))))

(defun acpe--session-save (conn)
  "Persist SESSION-ID for CONN in the local session store."
  (let* ((records (or (acpe--session-store-read) '()))
         (record `( :sessionId ,(acpe--session-id conn)
                    :agentName ,(acpe--agent-name conn)
                    :workingDirectory ,(acpe--working-directory conn)
                    :configOptions ,(acpe--config-options conn)
                    :firstPrompt ,(acpe--first-prompt conn)
                    :updatedAt ,(float-time))))
    (acpe--session-store-write
     (cons record (cl-remove (acpe--session-id conn) records
                             :key (lambda (entry) (plist-get entry :sessionId))
                             :test #'equal)))))

(defun acpe--session-new (conn)
  "Create a new session on CONN and store the session ID."
  (let* ((result (acpe--request
                  conn 'session/new
                  `( :cwd ,(acpe--working-directory conn)
                     :mcpServers [])))
         (id (plist-get result :sessionId)))
    (unless id
      (error "Session/new did not return a sessionId"))
    (setf (acpe--session-id conn) id
          (acpe--config-options conn)
          (append (plist-get result :configOptions))))
  (acpe--prompt-update-mode-line conn))

(defun acpe--session-load (conn session-id cwd)
  "Load an existing SESSION-ID on CONN."
  (let* ((result
          (acpe--request conn 'session/load
                         `( :sessionId ,session-id
                            :cwd ,cwd
                            :mcpServers [])))
         (existing (cl-find session-id (acpe--session-store-read)
                            :key (lambda (e) (plist-get e :sessionId))
                            :test #'equal)))
    (setf (acpe--session-id conn) session-id
          (acpe--config-options conn)
          (or (append (plist-get result :configOptions) nil)
              (plist-get existing :configOptions))
          (acpe--first-prompt conn) (plist-get existing :firstPrompt)))
  (acpe--prompt-update-mode-line conn))

(defun acpe--session-prompt (conn)
  "Prompt to resume a session for CONN."
  (when-let*
      ((sessions
        (cl-sort (cl-remove-if-not
                  (lambda (s)
                    (and (equal (plist-get s :agentName) (acpe--agent-name conn))
                         (equal (plist-get s :workingDirectory) (acpe--working-directory conn))))
                  (acpe--session-store-read))
                 #'> :key (lambda (s) (or (plist-get s :updatedAt) 0)))))
    (let* ((new-label "*New session*")
           (annotated (mapcar (lambda (s)
                                `(,(format-time-string
                                    "%Y-%m-%d %H:%M"
                                    (seconds-to-time (plist-get s :updatedAt)))
                                  . ,s))
                              sessions))
           (pairs
            `((,new-label . nil)
              ,@(mapcar (pcase-lambda (`(,name . ,s))
                          `(,name . ,(plist-get s :sessionId)))
                        annotated)))
           (collection
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity)
                             (annotation-function
                              . ,(lambda (key)
                                   (when-let* ((s (cdr (assoc key annotated)))
                                               (fp (plist-get s :firstPrompt))
                                               ((stringp fp)))
                                     (concat "  " (substring-no-properties fp))))))
                (complete-with-action action pairs string predicate)))))
      (cdr (assoc (completing-read
                   (format-prompt "Session" new-label)
                   collection nil t nil nil new-label)
                  pairs)))))

(defun acpe--prompt-update-mode-line (conn)
  "Force mode-line update in CONN's prompt buffer."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun acpe--build-prompt-content (text)
  "Convert TEXT to a content-block vector.
Spans with acpe-lazy-insert property become resource blocks."
  (let ((blocks nil)
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((lazy-insert (get-text-property pos 'acpe-lazy-insert text))
             (end (next-single-property-change pos 'acpe-lazy-insert text len)))
        (if lazy-insert
            (push (apply #'funcall lazy-insert) blocks)
          (let ((str (decode-coding-string
                      (substring-no-properties text pos end)
                      'utf-8 t)))
            (unless (string-empty-p str)
              (push `(:type "text" :text ,str) blocks))))
        (setq pos end)))
    (vconcat (nreverse blocks))))

;;; Commands

(defun acpe-send-prompt (conn text)
  "Send TEXT as a prompt to CONN."
  (unless (acpe--initialized-p conn)
    (user-error "Connection not initialized"))
  (unless (acpe--session-id conn)
    (user-error "No active session"))
  (unless (acpe--first-prompt conn)
    (setf (acpe--first-prompt conn) (substring-no-properties text)))
  (let* ((embedded-p
          (eq t (plist-get
                 (plist-get (acpe--capabilities conn) :promptCapabilities)
                 :embeddedContext)))
         (prompt (if embedded-p
                     (acpe--build-prompt-content text)
                   (vector `( :type "text"
                              :text ,(decode-coding-string
                                      (substring-no-properties text)
                                      'utf-8 t))))))
    (acpe--prompt-update-mode-line conn)
    (acpe--request
     conn 'session/prompt
     `( :sessionId ,(acpe--session-id conn)
        :prompt ,prompt)
     (lambda (result error)
       (acpe--flush-thought-chunks conn)
       (acpe--flush-message-chunks conn)
       (setf (acpe--cancelling conn) nil)
       (acpe--session-save conn)
       (when error
         (acpe--prompt-insert
          conn "\n" (propertize error 'font-lock-face 'error)))
       (when-let* ((reason (plist-get result :stopReason))
                   ((not (equal reason "end_turn" ))))
         (acpe--prompt-insert
          conn "\n" (propertize reason 'font-lock-face 'warning)))
       (acpe--prompt-update-mode-line conn)
       (run-hooks 'acpe-prompt-finished-hook)))))

(defun acpe-restart (conn)
  "Start a new session on CONN, discarding the current context."
  (interactive (list (acpe--live-connection)))
  (let* ((proc (jsonrpc--process conn))
         (command (process-command proc)))
    (jsonrpc-shutdown conn)
    (while (jsonrpc-running-p conn)
      (accept-process-output nil 0.1))
    (acpe--prompt-insert conn (propertize "\nrestarted" 'font-lock-face 'warning))
    (acpe (combine-and-quote-strings command))))

(defun acpe-insert (conn)
  "Insert a buffer reference for the current buffer into prompt.
CONN is inferred if called interactively w/o a prefix arg otherwise
prompted.  The visible @mention text is not sent; only the
acpe-lazy-insert text property spanning it is resolved at send time."
  (interactive
   (list
    (or (unless current-prefix-arg
          (acpe--live-connection t))
        (let ((collections
               (mapcar (lambda (c)
                         (cons (acpe--prompt-buffer-name c)
                               c))
                       acpe--connections)))
          (cdr (assoc (completing-read "Connection: " collections nil t)
                      collections)))
        (user-error "No live connections"))))
  (let* ((start (when (use-region-p)
                  (line-number-at-pos (region-beginning))))
         (end (when start
                (line-number-at-pos (1- (region-end)))))
         (filename (buffer-file-name))
         (target (if filename
                     (file-relative-name
                      filename (acpe--working-directory conn))
                   (format "<%s>" (buffer-name))))
         (lazy-insert (if filename
                          (list #'acpe--insert-file filename start end)
                        (list #'acpe--insert-buffer (buffer-name) start end)))
         (text (concat
                (propertize
                 (cond ((and start end (not (= start end)))
                        (format "@%s:%d:%d" target start end))
                       (start (format "@%s:%d" target start))
                       (t (format "@%s" target)))
                 'acpe-lazy-insert lazy-insert
                 'font-lock-face 'link
                 'rear-nonsticky t)
                " ")))
    (let ((buffer (acpe--output-buffer conn)))
      (unless (buffer-live-p buffer)
        (kill-new text)
        (user-error "No active prompt buffer - reference killed"))
      (pop-to-buffer buffer)
      (unless (and comint-last-prompt
                   (>= (point) (cdr comint-last-prompt)))
        (goto-char (point-max)))
      (unless (memq (char-before) '(?\s ?\t))
        (insert " "))
      (insert text))))

(defvar acpe-control-mode)              ; defined later; forward declaration

(defun acpe-cancel (conn)
  "Cancel the current turn on CONN, replying to pending permissions with cancelled."
  (interactive (list (acpe--live-connection)))
  (unless (acpe--session-id conn)
    (user-error "No active session"))
  (dolist (item (acpe--pending-permissions conn))
    (jsonrpc-connection-send conn
                             :id (plist-get item :id)
                             :method (plist-get item :method)
                             :result '(:outcome (:outcome "cancelled"))))
  (setf (acpe--pending-permissions conn) nil
        (acpe--cancelling conn) t)
  (jsonrpc-notify conn 'session/cancel
                  `(:sessionId ,(acpe--session-id conn)))
  (acpe--update-permission-queue-positions conn)
  (when acpe-control-mode
    (quit-window)))

(defun acpe-accept (conn &optional allow-always)
  "Accept the oldest pending permission on CONN.
ALLOW-ALWAYS sends allow_always instead of allow_once."
  (interactive (list (acpe--live-connection) current-prefix-arg))
  (let ((pending (acpe--pending-permissions conn)))
    (unless pending
      (user-error "No pending permission request"))
    (let* ((item (car pending))
           (opts (plist-get (plist-get item :deferred) :options))
           (kind (if allow-always "allow_always" "allow_once"))
           (chosen (cl-find kind opts
                            :key (lambda (o) (plist-get o :kind))
                            :test #'equal)))
      (when (and allow-always chosen
                 (not (yes-or-no-p "Always Allow? ")))
        (user-error "Aborting"))
      (setf (acpe--pending-permissions conn) (cdr pending))
      (if chosen
          (progn
            (jsonrpc-connection-send
             conn
             :id (plist-get item :id) :method (plist-get item :method)
             :result `(:outcome ( :outcome "selected"
                                  :optionId ,(plist-get chosen :optionId))))
            (message "Allowed"))
        (acpe-cancel conn)
        (message "No %s option in permission request `%s'"
                 kind (mapcar (lambda (o) (plist-get o :kind)) opts)))
      (acpe--update-permission-queue-positions conn)
      (when acpe-control-mode
        (quit-window)))))


;;; Connection management

(defun acpe--live-connection (&optional nowarn)
  "Return the active connection for current project, or error unless NOWARN."
  (let* ((conn
          (or acpe--connection
              (cl-find (expand-file-name
                        (if-let* ((proj (project-current)))
                            (project-root proj)
                          default-directory))
                       acpe--connections
                       :key #'acpe--working-directory
                       :test #'equal))))
    (cond ((and conn (jsonrpc-running-p conn)) conn)
          (nowarn nil)
          (t (user-error "No active acpe connection")))))

(defun acpe--on-shutdown (conn)
  "Clean up after CONN shuts down."
  (setq acpe--connections (delq conn acpe--connections))
  (let* ((agent-proc (ignore-errors (jsonrpc--process conn)))
         (status (and agent-proc (process-status agent-proc)))
         (code (and agent-proc (process-exit-status agent-proc))))
    (when (and (buffer-live-p (acpe--output-buffer conn))
               (or (eq status 'signal)
                   (and (eq status 'exit) code (not (zerop code)))))
      (with-current-buffer (acpe--output-buffer conn)
        (setq mode-line-process
              `(:propertize ,(format ":%s [%s]"
                                     (if (eq status 'signal) "signal" "exit")
                                     code)
                            font-lock-face error)))))
  (message "[acpe] Connection to %s closed" (acpe--agent-name conn)))


;;; Prompt

(defconst acpe--input-prompt "> "
  "Input prompt string for `acpe-prompt-mode'.")

(defvar acpe--prompt-commands
  '(("/cancel" acpe-cancel    "Cancel the current turn")
    ("/help"   acpe--cmd-help "Show available slash commands"))
  "List of prompt buffer slash commands (NAME FN DESCRIPTION).")

(defun acpe--cmd-help (conn)
  "Output available slash commands to CONN's prompt buffer."
  (acpe--prompt-insert
   conn
   (concat
    "\nAcpe Commands:\n"
    (mapconcat (lambda (cmd)
                 (format "  %-12s %s\n" (car cmd) (nth 2 cmd)))
               acpe--prompt-commands
               "")
    (when-let* ((agent-cmds (acpe--slash-commands conn)))
      (concat "Agent commands:\n"
              (mapconcat (lambda (c)
                           (format "  %-12s %s\n"
                                   (concat "/" (plist-get c :name))
                                   (or (plist-get c :description) "")))
                         agent-cmds
                         ""))))))

(defun acpe--prompt-dispatch-command (conn text)
  "Dispatch slash command TEXT for CONN."
  (let* ((name (string-trim text))
         (local (assoc name acpe--prompt-commands)))
    (cond (local (funcall (cadr local) conn))
          ((cl-find name (acpe--slash-commands conn)
                    :key (lambda (c)
                           (concat "/" (plist-get c :name)))
                    :test #'equal)
           (acpe-send-prompt conn text))
          (t (acpe--prompt-insert
              conn
              (propertize (format "\nUnknown command: %s\n" name)
                          'font-lock-face 'error))))))

(defun acpe--prompt-command-capf ()
  "Completion-at-point for slash commands in the prompt input line."
  (when-let*
      ((prompt-end (and comint-last-prompt
                        (cdr comint-last-prompt)))
       ((>= (point) prompt-end))
       (input-start (save-excursion
                      (goto-char prompt-end)
                      (skip-chars-forward " \t")
                      (point)))
       ((eq (char-after input-start) ?/))
       (bounds (cons input-start (point)))
       (collection
        (append
         (mapcar (lambda (cmd) (cons (car cmd) (nth 2 cmd)))
                 acpe--prompt-commands)
         (mapcar (lambda (c)
                   (cons (concat "/" (plist-get c :name))
                         (plist-get c :description)))
                 (and acpe--connection
                      (acpe--slash-commands acpe--connection))))))
    (list (car bounds) (cdr bounds) collection
          :exclusive 'no
          :annotation-function
          (lambda (str) (cdr (assoc (substring-no-properties str) collection)))
          :company-prefix-length t)))

(defun acpe-mode-next (conn &optional reverse-p)
  "Cycle CONN's agent mode to the next option; REVERSE-P goes backward."
  (interactive (list (acpe--live-connection)))
  (unless (acpe--session-id conn)
    (user-error "No active session"))
  (let* ((opts (or (acpe--config-options conn)
                   (user-error "Agent exposes no config options")))
         (opt (or (cl-find "mode" opts
                           :key (lambda (o) (plist-get o :id))
                           :test #'equal)
                  (user-error "Agent exposes no \"mode\" config option")))
         (options (append (plist-get opt :options) nil))
         (values (mapcar (lambda (v) (plist-get v :value)) options))
         (cur-idx (or (cl-position (plist-get opt :currentValue) values :test #'equal) 0))
         (dir (if reverse-p #'1- #'1+))
         (next-val (nth (mod (funcall dir cur-idx) (length values)) values))
         (result (acpe--request
                  conn 'session/set_config_option
                  `( :sessionId ,(acpe--session-id conn)
                     :configId  ,(plist-get opt :id)
                     :value     ,next-val))))
    (setf (acpe--config-options conn)
          (append (plist-get result :configOptions) nil))
    (acpe--prompt-update-mode-line conn)
    (message "New mode `%s'" next-val)))

(defun acpe-mode-previous (conn)
  "Cycle CONN's agent mode backward; see `acpe-mode-next'."
  (interactive (list (acpe--live-connection)))
  (acpe-mode-next conn t))

(defvar-keymap acpe-control-mode-map
  :doc "Keymap for `acpe-control-mode'."
  "C-c C-c"    #'acpe-accept
  "C-c C-k"    #'acpe-cancel
  "C-c C-n"    #'acpe-restart
  "C-c C-m"    #'acpe-mode-next
  "C-c C-M"    #'acpe-mode-previous)

(define-minor-mode acpe-control-mode
  "Minor mode providing acpe commands in prompt, diff, and plan buffers."
  :keymap acpe-control-mode-map
  (when acpe-control-mode
    (setq-local quit-window-kill-buffer t)))

(defvar-keymap acpe-mode-cycle-repeat-map
  :doc "Repeat map for `acpe-mode-next'/`acpe-mode-previous'."
  :repeat t
  "m" #'acpe-mode-next
  "M" #'acpe-mode-previous)

(defvar-keymap acpe-prompt-mode-map
  :doc "Keymap for `acpe-prompt-mode'."
  "S-<return>" #'newline
  "C-c C-c"    #'acpe-accept
  "C-c C-k"    #'acpe-cancel
  "C-c C-n"    #'acpe-restart
  "TAB"        #'acpe-toggle-detail-at-point
  "C-c C-m"    #'acpe-mode-next
  "C-c C-M"    #'acpe-mode-previous)

(define-derived-mode acpe-prompt-mode comint-mode "ACP"
  "Major mode for interacting with an ACP agent."
  :interactive nil
  (setq-local comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t
              comint-input-sender #'acpe--prompt-input-sender
              comint-prompt-regexp (concat "^" (regexp-quote acpe--input-prompt))
              comint-process-echoes nil
              scroll-conservatively 101
              acpe--prompt-overlay-arrow (make-marker)
              buffer-invisibility-spec (cons '(acpe-detail . t)
                                             buffer-invisibility-spec))
  (add-to-list 'overlay-arrow-variable-list 'acpe--prompt-overlay-arrow)
  (add-hook 'comint-input-filter-functions
            (lambda (input) (not (string-prefix-p "/" (string-trim input))))
            nil t)
  (add-hook 'completion-at-point-functions #'acpe--prompt-command-capf nil t)
  ;; Comint requires a live process in the buffer; start a dummy one.
  (unless (comint-check-proc (current-buffer))
    (let ((proc (start-process "acpe-prompt" (current-buffer) nil)))
      (add-hook 'kill-buffer-hook (lambda () (delete-process proc)) nil t)
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc #'comint-output-filter)
      (set-marker (process-mark proc) (point-max))
      (comint-output-filter proc acpe--input-prompt)
      (when comint-last-prompt
        (setq acpe--output-end
              (copy-marker (car comint-last-prompt) t))))))

(defun acpe--prompt-input-sender (_proc input)
  "Comint input-sender; dispatch INPUT for the current connection."
  (let ((conn acpe--connection)
        (text (string-trim input)))
    (acpe--emit-input-prompt conn)
    (cond ((string-empty-p text))
          ((string-prefix-p "/" text)
           (acpe--prompt-dispatch-command conn text))
          ((not (and conn (jsonrpc-running-p conn)))
           (acpe--prompt-insert
            conn (propertize "\nNo active connection.\n"
                             'font-lock-face 'error)))
          (t (acpe-send-prompt conn text)))))

(defun acpe--prompt-insert (conn &rest args)
  "Insert ARGS into CONN's prompt buffer before the input prompt."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (or acpe--output-end (point-max)))
          (apply #'insert args))
        ;; Keep comint's input-start marker aligned.
        (when (and comint-last-prompt acpe--output-end)
          (move-marker (car comint-last-prompt) acpe--output-end))))))

(defun acpe--emit-input-prompt (conn)
  "Re-emit the input prompt via CONN's dummy comint process."
  (when-let* ((buffer (acpe--output-buffer conn))
              ((buffer-live-p buffer))
              (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (comint-output-filter proc acpe--input-prompt)
      ;; Reset acpe--output-end before the new indicator.
      (when comint-last-prompt
        (if acpe--output-end
            (set-marker acpe--output-end (1- (car comint-last-prompt)))
          (setq acpe--output-end
                (copy-marker (1- (car comint-last-prompt)) t)))))))

(defun acpe--prompt-buffer-name (conn)
  "Return the prompt buffer name for CONN."
  (let ((default-directory (acpe--working-directory conn)))
    (project-prefixed-buffer-name "acpe")))

(defun acpe--make-prompt-buffer (conn)
  "Create the prompt buffer for CONN and return it."
  (let* ((base-name (acpe--prompt-buffer-name conn))
         (name (if-let* ((existing (get-buffer base-name))
                         (old-conn (buffer-local-value 'acpe--connection existing))
                         ((jsonrpc-running-p old-conn)))
                   (generate-new-buffer-name base-name)
                 base-name))
         (new-buffer-p (not (get-buffer name)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'acpe-prompt-mode)
        (acpe-prompt-mode))
      (setq-local
       acpe--connection conn
       mode-line-process
       `((:eval
          (if (and acpe--connection
                   (> (jsonrpc-continuation-count acpe--connection) 0))
              ,(propertize ":%s" 'face 'warning)
            ":%s"))
         (acpe--connection
          (" "
           (:eval
            (when (acpe--pending-permissions acpe--connection)
              ,(propertize "?" 'face 'error
                           'mouse-face 'mode-line-highlight
                           'help-echo "mouse-1: accept permission"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line mouse-1] #'acpe-accept)
                                        map))))
           " ["
           (:eval
            (when-let* ((opts (acpe--config-options acpe--connection))
                        (mode-opt (cl-find "mode" opts
                                           :key (lambda (o) (plist-get o :id))
                                           :test #'equal))
                        (cur-val (plist-get mode-opt :currentValue))
                        (options (append (plist-get mode-opt :options) nil))
                        (cur-opt (cl-find cur-val options
                                          :key (lambda (v) (plist-get v :value))
                                          :test #'equal)))
              (plist-get cur-opt :name)))
           "]"))))
      (add-hook 'kill-buffer-hook
                (lambda () (jsonrpc-shutdown acpe--connection))
                nil t)
      (setf (acpe--output-buffer conn) buffer)
      (when new-buffer-p
        (acpe--prompt-insert
         conn (concat
               "Welcome to Acpe!\n"
               "Type `/help' for a list of commands.\n"
               "\n"
               (when-let* ((prompt (acpe--first-prompt conn)))
                 (format "Session `%s' continued...\n\n" prompt)))))
      (acpe--prompt-update-mode-line conn))))


;;; Entry point

;;;###autoload
(defun acpe (cmd &optional dir existing)
  "Start an ACP session with agent command CMD.
DIR defaults to project root or `default-directory'.
EXISTING reuses a live connection instead of spawning a new process.

Interactively, prompt for a command.  With \\[universal-argument], always
start a new session even if one exists for the current project."
  (interactive
   (let ((existing (unless current-prefix-arg (acpe--live-connection 'nowarn))))
     (list (unless existing
             (read-shell-command "Agent command: " (car acpe-command-history)
                                 'acpe-command-history))
           nil
           existing)))
  (if-let* ((existing (or existing
                          (unless current-prefix-arg
                            (acpe--live-connection 'nowarn)))))
      ;; Live connection exists, show its buffer.
      (pop-to-buffer (acpe--output-buffer existing))
    (let* ((cmd-list (split-string-and-unquote cmd))
           (name (car cmd-list))
           (working-dir (expand-file-name
                         (or dir
                             (when-let* ((proj (project-current)))
                               (project-root proj))
                             default-directory)))
           (conn
            (make-instance
             'acpe-connection
             :name name
             :agent-name name
             :working-directory working-dir
             :request-dispatcher
             (lambda (conn method params)
               (apply #'acpe-handle-request conn method (append params nil)))
             :notification-dispatcher
             (lambda (conn method params)
               (apply #'acpe-handle-notification conn method (append params nil)))
             :on-shutdown #'acpe--on-shutdown
             :process
             (lambda (_c)
               (make-process
                :name name
                :command cmd-list
                :buffer (generate-new-buffer (format " *%s output*" name))
                :stderr (get-buffer-create (format " *%s stderr*" name))
                :connection-type 'pipe
                :noquery t
                :coding 'utf-8-unix)))))
      (push conn acpe--connections)
      ;; Initialize and open session, then show buffer.
      (condition-case err
          (progn
            (acpe--initialize conn)
            (if-let* ((session-id (acpe--session-prompt conn)))
                (acpe--session-load conn session-id working-dir)
              (acpe--session-new conn))
            (acpe--make-prompt-buffer conn)
            (pop-to-buffer (acpe--output-buffer conn))
            (message "[acpe] Connected to %s (session %s)"
                     name (acpe--session-id conn)))
        (t
         (jsonrpc-shutdown conn)
         (signal (car err) (cdr err)))))))

(define-key ctl-x-map "\C-r" 'acpe)
(define-key ctl-x-map "@" 'acpe-insert)

(provide 'acpe)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; acpe.el ends here
