;;; sludge.el --- background Lisp interaction -*- lexical-binding: t -*-

;;; Commentary:

;; The SLUDGE server runs in a Common Lisp system, which will generally be
;; running as an inferior-lisp process. To start the system, we (Emacs) tell
;; the Lisp to start the server and that it should listen at an address that
;; we provide.

;; Each buffer that wishes to communicate with the server gets its own
;; connection in the form of a network process object stored in the
;; buffer-local variable `sludge-process'. The value of that variable in
;; the inferior lisp buffer is the "master" process, in the sense that
;; future connections will be made to the same address as that one.

(make-variable-buffer-local
 (defvar sludge-mode nil
   "True if SLUDGE mode is enabled."))

(make-variable-buffer-local
 (defvar sludge-process nil
   "Connection to SLUDGE server."))

(defvar sludge-poll-rate 0.1)
(defvar sludge-max-retries 5)

(defvar sludge-default-address-format "/tmp/sludge-%d"
  "Format string used to generate the default SLUDGE address.
Should contain a numeric format operator, for which the Lisp
process id will be provided as argument.")

(defun sludge-default-address ()
  (format sludge-default-address-format (process-id (sludge-lisp-proc))))

(defun sludge-mode (&optional arg)
    "Toggle SLUDGE mode.
The usual minor mode convention applies for the argument: a positive numeric
argument means enable, negative means disable, and no argument toggles.

When SLUDGE mode is enabled, a connection to a running SLUDGE server will
be made and used for background interaction with a Common Lisp system."
  (interactive (list (or current-prefix-arg 'toggle)))
  (cond ((if (eq arg 'toggle)
             (not sludge-mode)
             (> (prefix-numeric-value arg) 0))
         (condition-case err
             (setq sludge-process
                   (let ((master (sludge-master-process)))
                     (if master
                         (sludge-connect (process-contact master))
                         (sludge-start-server (sludge-default-address)))))
           (error (setq sludge-mode nil)
                  (signal (car err) (cdr err))))
         (setq sludge-mode t)
         (run-hooks 'sludge-mode-hooks)
         (when (called-interactively-p 'interactive)
           (message "SLUDGE mode enabled")))
        (t (when (and sludge-process (process-live-p sludge-process))
             (if (and (eq sludge-process (sludge-master-process))
                      (yes-or-no-p "Shut down SLUDGE server, too? "))
                 (sludge-stop-server)
                 (delete-process sludge-process)))
           (setq sludge-process nil
                 sludge-mode nil)
           (when (called-interactively-p 'interactive)
             (message "SLUDGE mode disabled"))))
  (force-mode-line-update)
  sludge-mode)

(defvar sludge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'sludge-show-arglist)
    (define-key map "\C-c\C-f" 'sludge-show-function-documentation)
    (define-key map "\C-c\C-v" 'sludge-show-variable-documentation)
    (define-key map "\C-c\C-d" 'sludge-describe-symbol)
    map)
  "Keymap for SLUDGE minor mode.")

(add-minor-mode 'sludge-mode " SLUDGE" sludge-mode-map)

(defun sludge-lisp-proc ()
  (cond ((and (fboundp 'inferior-lisp-proc) inferior-lisp-buffer)
         (inferior-lisp-proc))
        (t (error "Can't find Lisp process"))))

(defmacro with-sludge-lisp-buffer (&rest body)
  `(with-current-buffer inferior-lisp-buffer ,@body))

(defun sludge-start-server-command (address)
  (format "(sludge:start-server :address %S)\n" address))

(defun sludge-stop-server-command ()
  "(sludge:stop-server)")

(defun sludge-send-lisp-string (string &optional process)
  (comint-send-string (or process (sludge-lisp-proc)) string))

(defun sludge-start-server (address)
  "Start the SLUDGE server in an inferior Lisp and connect to it.
Sets the inferior Lisp buffer's `sludge-process' variable to the \"master\"
connection (i.e., the one from which all others will be cloned)."
  (interactive (list (sludge-default-address)))
  (sludge-send-lisp-string (sludge-start-server-command address))
  (with-sludge-lisp-buffer
    (setq sludge-process (sludge-try-connect address))))

(defun sludge-stop-server ()
  "Stop the SLUDGE server and disconnect all clients."
  (interactive)
  ;; This should be a protocol message.
  (sludge-send-lisp-string (sludge-stop-server-command))
  (message "SLUDGE server stopped")
  (with-sludge-lisp-buffer
    (when sludge-process
      (delete-process sludge-process)
      (setq sludge-process nil))))

(defun sludge-master-process ()
  (ignore-errors (with-sludge-lisp-buffer sludge-process)))

(defun sludge-try-connect (address)
  "Repeatedly attempt to connect to the SLUDGE server at ADDRESS."
  (or (catch 'connected
        (dotimes (i sludge-max-retries)
          (sit-for sludge-poll-rate)
          (message "Polling %s" address)
          (let ((proc (ignore-errors (sludge-connect address))))
            (when proc
              (message "Connected to SLUDGE server at %s" address)
              (throw 'connected proc)))))
      (error "Timed out connecting to SLUDGE server at %s" address)))

(defun sludge-connect (address)
  "Connect to the SLUDGE server at ADDRESS."
  (sludge-init-process
   (cond ((and (listp address) (car address))
          (make-network-process :name "sludge"
                                :host (car address)
                                :service (cadr address)
                                :noquery t
                                :coding 'utf-8-unix
                                :filter 'sludge-process-reply))
         ((or (stringp address)
              (and (listp address) (setq address (cadr address))))
          (make-network-process :name "sludge"
                                :family 'local
                                :service address
                                :noquery t
                                :coding 'utf-8-unix
                                :filter 'sludge-process-reply))
         (t (error "Can't connect to SLUDGE server at %s" address)))))

;;;; The SLUDGE Protocol.

;;; These must match the definitions on the server side, or else there will
;;; be much wailing and gnashing of teeth.

(defun make-sludge-request (code tag &rest args)
  (apply #'list code tag args))

(defun sludge-response-code (message) (nth 0 message))
(defun sludge-request-code (message) (nth 1 message))
(defun sludge-response-tag (message) (nth 2 message))
(defun sludge-response-args (message) (nthcdr 3 message))

;;; In the SLUDGE protocol, we send requests from Emacs to the Lisp server,
;;; and possibly receive responses to those requests. Requests are tagged
;;; with client-generated integer identifiers. For each connection to a
;;; server, we'll keep our pending requests in a hash table keyed on
;;; request codes, with values of the form (TAG OK ERR). If a response
;;; comes in with a tag less than TAG, we ignore it; otherwise, we invoke
;;; either OK or ERR with the arguments from the response message.
;;; The pending requests table and tag counter are both stored in the
;;; connection process's plist, so there's no chance of confusion about
;;; which messages belong to what process.

(defun sludge-init-process (process)
  (process-put process 'sludge-pending-requests (make-hash-table))
  (process-put process 'sludge-tag-counter 1)
  process)

(defun sludge-pending-requests (process)
  "Return the pending request hash table for PROCESS."
  (process-get process 'sludge-pending-requests))

(defun sludge-tag-counter (process)
  "Return and increment the request tag counter for PROCESS."
  (let ((tag (process-get process 'sludge-tag-counter)))
    (process-put process 'sludge-tag-counter (1+ tag))
    tag))

(defun sludge-default-error-handler (error-symbol msg &rest args)
  (error "%s: %s" error-symbol msg))

(defun sludge-set-callbacks (process code tag ok &optional err)
  (puthash code
           (list tag ok (or err 'sludge-default-error-handler))
           (sludge-pending-requests process)))

(defun sludge-handle-response (process response)
  (let ((code (sludge-response-code response))
        (request-code (sludge-request-code response))
        (response-tag (sludge-response-tag response))
        (args (sludge-response-args response)))
    (let ((entry (or (gethash request-code (sludge-pending-requests process))
                     (error "Unexpected response from Lisp: %s" response))))
      (let ((request-tag (nth 0 entry))
            (ok (nth 1 entry))
            (err (nth 2 entry)))
        (cond ((< response-tag request-tag)) ; ignore superseded request
              ((= response-tag request-tag)
               (unwind-protect
                    (if (eq code :ok)
                        (apply ok args)
                        (apply err args))
                 (remhash request-code (sludge-pending-requests process))))
              (t (error "Unexpected tag in response: %s" response)))))))

(defun sludge-send-request (process request)
  (process-send-string process (format "%S\n" request)))

(defun sludge-async-request (process code args ok &optional err)
  (unless (and (setq process (or process (sludge-master-process)))
               (process-live-p process))
    (error "Invalid SLUDGE process"))
  (let ((tag (sludge-tag-counter process)))
    (sludge-set-callbacks process code tag ok err)
    (sludge-send-request process (apply #'make-sludge-request code tag args))))

(defun sludge-request (process code &rest args)
  "Make a synchronous request to the SLUDGE server at PROCESS."
  (catch 'done
    (sludge-async-request process code args
                          (lambda (&rest args) (throw 'done args))
                          (lambda (&rest args) (throw 'done nil)))
    (let ((debug-on-quit t)
          (inhibit-quit nil))
      (while (process-live-p process)
        (accept-process-output process 0.01)))))

(defun sludge-process-reply (process string)
  "The SLUDGE process filter function.
Reads a response from the Lisp and handles it."
  (let ((form (read string)))
    (cond ((and (consp form)
                (>= (length form) 3)
                (member (nth 0 form) '(:ok :error))
                (symbolp (nth 1 form))
                (numberp (nth 2 form)))
           (sludge-handle-response process form))
          (t (error "Invalid response from Lisp: %s" form)))))

;;;; Package handling.

;; Adapted from `slime-search-buffer-package'.
(defun sludge-scry-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (let ((match (match-string-no-properties 2)))
          (when match (ignore-errors (read match))))))))

(defun sludge-get-buffer-package ()
  "Return and cache the package for the current buffer."
  (or (ignore-errors (buffer-local-value 'sludge-package (current-buffer)))
      (ignore-errors (buffer-local-value 'package (current-buffer)))
      (set (make-local-variable 'sludge-package)
           (or (sludge-scry-buffer-package)
               "COMMON-LISP-USER"))))

(defun sludge-in-package (name)
  "Set the current Common Lisp package to NAME."
  (interactive "MPackage name: ")
  (let ((buffer (current-buffer)))
    (sludge-async-request sludge-process
                          :in-package (list name)
                          (lambda (name)
                            (with-current-buffer buffer
                              (set (make-local-variable 'sludge-package) name)))
                          (lambda (condition message)
                            (message message)))))

;;;; Arglist handling.

;;; We keep a per-buffer, one-element cache of the most recently fetched
;;; arglist. This should help reduce request traffic in many cases.

(defun sludge-cache-arglist (fn &rest arglist)
  (when sludge-process
    (process-put sludge-process
                 'sludge-last-arglist
                 (nconc (list fn) arglist))))

(defun sludge-last-arglist ()
  (when sludge-process
    (process-get sludge-process 'sludge-last-arglist)))

;;; To avoid reinventing the wheel yet again, we'll hook into ElDoc and use
;;; its message display code.

(defun sludge-documentation-function ()
  "Display a documentation string for the function at or around point.
Intended to be used as a value for `eldoc-documentation-function'."
  (when sludge-process
    (let ((symbol (lisp-fn-called-at-pt)))
      (when symbol
        (let ((cache (sludge-last-arglist)))
          (if (eq symbol (car cache))
              (when (cdr cache)
                (eldoc-message (apply #'make-arglist-string cache)))
              (sludge-show-arglist symbol)))))))

(defun sludge-show-arglist (&optional fn)
  (interactive (lisp-symprompt "Function argument list" (lisp-fn-called-at-pt)))
  (setq fn (ensure-symbol fn))
  (sludge-cache-arglist fn)
  (sludge-async-request sludge-process
                        :arglist (list fn)
                        (lambda (arglist)
                          (sludge-cache-arglist fn arglist)
                          (eldoc-message (make-arglist-string fn arglist)))
                        #'ignore))

(defun ensure-string (object)
  (cond ((stringp object) object)
        ((symbolp object) (symbol-name object))
        (t (error "Not a string designator: %S" object))))

(defun parse-cl-symbol (token)
  "Break the given token up into a package prefix and a symbol name
and return them as a single cons cell.

Assumes that `:' is the package marker, `\\' is the (unique) single
escape character, and `|' is the (unique) multiple escape character.
Makes no attempt to deal with potential numbers or macro characters."
  (let ((token (ensure-string token))
        (escaping nil)
        (marker (cons nil nil))
        (i 0))
    (while (< i (length token))
      (let ((c (aref token i)))
        (cond ((char-equal c ?\\)
               (setq i (1+ i)))
              ((char-equal c ?|)
               (setq escaping (not escaping)))
              ((and (not escaping) (char-equal c ?:))
               (cond ((null (car marker))
                      (rplaca marker i))
                     ((and (null (cdr marker)) (= (car marker) (1- i)))
                      (rplacd marker i))
                     (t (error "Too many colons in %S" token))))))
      (setq i (1+ i)))
    (cond ((null (car marker)) (cons nil token))
          ((zerop (car marker)) (cons "keyword" token))
          (t (cons (substring token 0 (car marker))
                   (substring token (1+ (or (cdr marker) (car marker)))))))))

;;; This auxiliary function (shamelessly stolen from CLtL2, Appendix C) is
;;; like `mapcar' but has two extra purposes: (1) it handles dotted lists;
;;; (2) it tries to make the result share with the argument X as much as
;;; possible.

(defun maptree (fn x)
  (if (atom x)
      (funcall fn x)
      (let ((a (funcall fn (car x)))
            (d (maptree fn (cdr x))))
        (if (and (eql a (car x)) (eql d (cdr x)))
            x
            (cons a d)))))

(defun drop-package-prefix (symbol)
  "Return just the symbol name, without any package prefix."
  (condition-case nil
      (intern (cdr (parse-cl-symbol symbol)))
    (error symbol)))

(defun sludge-pretty-arglist (object)
  "Recursively drop package prefixes from symbols in the given arglist."
  (cond ((symbolp object) (drop-package-prefix object))
        ((listp object) (maptree #'sludge-pretty-arglist object))
        (t object)))

(defun make-arglist-string (fn arglist)
  (format "%S" (cons fn (sludge-pretty-arglist arglist))))

(defun sludge-setup-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function)
       'sludge-documentation-function))

(add-hook 'sludge-mode-hooks 'sludge-setup-eldoc)

;;;; Documentation handling.

(defun sludge-show-documentation (name doc-type)
  (sludge-async-request sludge-process
                        :documentation (list name doc-type)
                        (lambda (docstring)
                          (eldoc-message docstring))
                        #'ignore))

(defun ensure-symbol (object)
  (cond ((stringp object) (intern object))
        ((symbolp object) object)
        (t (error "Not a symbol designator: %S" object))))

(defun sludge-show-function-documentation (&optional fn)
  (interactive (lisp-symprompt "Function documentation" (lisp-fn-called-at-pt)))
  (sludge-show-documentation (ensure-symbol fn) 'function))

(defun sludge-show-variable-documentation (&optional var)
  (interactive (lisp-symprompt "Variable documentation" (lisp-var-at-pt)))
  (sludge-show-documentation (ensure-symbol var) 'variable))

(defun sludge-describe-symbol (&optional symbol)
  (interactive (lisp-symprompt "Describe" (lisp-var-at-pt)))
  (sludge-async-request sludge-process
                        :describe (list (ensure-symbol symbol))
                        (lambda (description)
                          (with-output-to-temp-buffer "*describe*"
                            (princ description)))))

;;;; Very simple symbol completion.

(defun sludge-symbol-completions (symbol)
  (mapcar 'symbol-name
          (sludge-request sludge-process
                          :symbol-completions (upcase (ensure-string symbol)))))

(defun sludge-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start end (sludge-symbol-completions
                       (buffer-substring-no-properties start end))))))

(defun sludge-setup-completion ()
  (add-hook 'completion-at-point-functions
            'sludge-completion-at-point
            nil 'local))

(add-hook 'sludge-mode-hooks 'sludge-setup-completion)
