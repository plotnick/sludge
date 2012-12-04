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

(defvar sludge-default-address "/tmp/sludge")

(defvar sludge-poll-rate 0.1)
(defvar sludge-max-retries 5)

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
                         (sludge-start-server sludge-default-address))))
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

(add-minor-mode 'sludge-mode " SLUDGE")

(defun sludge-start-server-command (address)
  (format "(sludge:start-server :address %S)\n" address))

(defun sludge-stop-server-command ()
  "(sludge:stop-server)")

(defun sludge-start-server (address)
  "Start the SLUDGE server in an inferior Lisp and connect to it.
Sets the inferior Lisp buffer's `sludge-process' variable to the \"master\"
connection (i.e., the one from which all others will be cloned)."
  (interactive (list sludge-default-address))
  (cond (inferior-lisp-buffer
         (comint-send-string (inferior-lisp-proc)
                             (sludge-start-server-command address)))
        (t (error "Can't find Lisp process")))
  (with-current-buffer inferior-lisp-buffer
    (setq sludge-process (sludge-try-connect address))))

(defun sludge-stop-server ()
  "Stop the SLUDGE server and disconnect all clients."
  (interactive)
  ;; This should be a protocol message.
  (cond (inferior-lisp-buffer
         (comint-send-string (inferior-lisp-proc)
                             (sludge-stop-server-command)))
        (t (error "Can't find Lisp process")))
  (message "SLUDGE server stopped")
  (with-current-buffer inferior-lisp-buffer
    (when sludge-process
      (delete-process sludge-process)
      (setq sludge-process nil))))

(defun sludge-master-process ()
  (ignore-errors (with-current-buffer inferior-lisp-buffer sludge-process)))

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
        (t (error "Can't connect to SLUDGE server at %s" address))))

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
;;; with client-generated integer identifiers. We'll keep our pending
;;; requests in a hash table, keyed on request codes, with values of the
;;; form (TAG OK ERR). If a response comes in with a tag less than TAG,
;;; we ignore it; otherwise, we invoke either OK or ERR with the arguments
;;; from the response message.

(defvar sludge-pending-requests (make-hash-table))
(defvar sludge-tag-counter 0)

(defun sludge-clear-pending-requests ()
  (clrhash sludge-pending-requests))

(defun sludge-default-error-handler (error-symbol msg &rest args)
  (error "%s: %s" error-symbol msg))

(defun sludge-set-callbacks (code tag ok &optional err)
  (let ((err (or err #'sludge-default-error-handler)))
    (puthash code (list tag ok err) sludge-pending-requests)))

(defun sludge-handle-response (response)
  (let ((code (sludge-response-code response))
        (request-code (sludge-request-code response))
        (response-tag (sludge-response-tag response))
        (args (sludge-response-args response)))
    (let ((entry (or (gethash request-code sludge-pending-requests)
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
                 (remhash request-code sludge-pending-requests)))
              (t (error "Unexpected tag in response: %s" response)))))))

(defun sludge-send-request (proc request)
  (process-send-string proc (format "%S\n" request)))

(defun sludge-async-request (proc code args ok &optional err)
  (let ((tag (setq sludge-tag-counter (1+ sludge-tag-counter))))
    (sludge-set-callbacks code tag ok err)
    (sludge-send-request proc (apply #'make-sludge-request code tag args))))

(defun sludge-request (proc code &rest args)
  "Make a synchronous request to the SLUDGE server at PROC."
  (catch 'done
    (sludge-async-request proc code args
                          (lambda (&rest args) (throw 'done args))
                          (lambda (&rest args) (throw 'done nil)))
    (let ((debug-on-quit t)
          (inhibit-quit nil))
      (while (process-live-p proc)
        (accept-process-output proc 0.01)))))

(defun sludge-process-reply (proc string)
  "The SLUDGE process filter function.
Reads a response from the Lisp and handles it."
  (let ((form (read string)))
    (cond ((and (consp form)
                (>= (length form) 3)
                (member (nth 0 form) '(:ok :error))
                (symbolp (nth 1 form))
                (numberp (nth 2 form)))
           (sludge-handle-response form))
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
  (let ((buffer (current-buffer)))
    (sludge-async-request sludge-process
                          :in-package (list name)
                          (lambda (name)
                            (with-current-buffer buffer
                              (set (make-local-variable 'sludge-package) name)))
                          (lambda (condition message)
                            (message message)))))

;;;; Arglist handling.

;;; This is the whole reason this system exists. To avoid reinventing the
;;; wheel yet again, we'll hook into ElDoc and use its arglist display code.

(defvar sludge-last-arglist nil
  "A one-element cache for function arglists.")

(defun sludge-documentation-function ()
  "Display a documentation string for the function at or around point.
Intended to be used as a value for `eldoc-documentation-function'."
  (when sludge-process
    (let ((symbol (lisp-fn-called-at-pt)))
      (when symbol
        (if (eq symbol (car sludge-last-arglist))
            (when (cdr sludge-last-arglist)
              (eldoc-message (apply #'make-arglist-string sludge-last-arglist)))
            (sludge-arglist symbol))))))

(defun sludge-arglist (&optional symbol)
  (setq sludge-last-arglist (list symbol))
  (sludge-async-request sludge-process
                        :arglist (list (or symbol (lisp-fn-called-at-pt)))
                        (lambda (symbol arglist)
                          (setq sludge-last-arglist (list symbol arglist))
                          (eldoc-message (make-arglist-string symbol arglist)))
                        #'ignore))

(defun make-arglist-string (fn arglist)
  (format "%S" (cons fn arglist)))

(defun sludge-setup-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function)
       'sludge-documentation-function))

(add-hook 'sludge-mode-hooks 'sludge-setup-eldoc)
