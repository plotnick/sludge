(defvar sludge nil
  "Global SLUDGE process.")

(defun sludge-connect (address &optional coding)
  (let ((proc (make-network-process :name "sludge"
                                    :family 'local
                                    :server nil
                                    :noquery t
                                    :service address
                                    :coding (or coding 'utf-8-unix))))
    (set-process-filter proc 'sludge-process-reply)
    (setq sludge proc)))

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

;;;; Arglist handling.

;;; This is the whole reason this system exists. To avoid reinventing the
;;; wheel yet again, we'll hook into ElDoc and use its arglist display code.

(defun sludge-documentation-function ()
  "Return a documentation string for the symbol at or around point.
Intended to be used as a value for `eldoc-documentation-function'."
  (let ((symbol (lisp-fn-called-at-pt)))
    (when symbol
      (let ((arglist (ignore-errors (sludge-request sludge :arglist symbol))))
        (when arglist
          (make-arglist-doc-string symbol arglist))))))

(defun make-arglist-doc-string (fn arglist)
  (format "%s: %S" fn arglist))

(defun sludge-arglist (&optional symbol)
  (sludge-async-request sludge
                        :arglist (list (or symbol (lisp-fn-called-at-pt)))
                        (lambda (symbol arglist)
                          (message "%s: %S" symbol arglist))
                        #'ignore))
