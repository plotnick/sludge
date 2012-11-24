\def\sludge{{\sc sludge}}

@*\sludge. \sludge is the {\sc s}imple {\sc l}isp {\sc u}sage and
{\sc d}ocumentation {\sc g}athering {\sc e}ngine.

@l
(provide "SLUDGE")
@e
(defpackage "SLUDGE"
  (:use "COMMON-LISP"
        "SB-BSD-SOCKETS"
        "SB-THREAD")
  (:export))
@e
(in-package "SLUDGE")

@t*Test suite. The test suite for this system uses Richard Waters's
{\sc rt} library. For more information on {\sc rt}, see Richard C.~Waters,
``Supporting the Regression Testing of Lisp Programs,''
{\it SIGPLAN Lisp Pointers}~4, no.~2 (1991): 47--53.

We use the sleazy trick of manually importing the external symbols of
the {\sc rt} package instead of the more sensible |(use-package "RT")|
because many compilers issue warnings when the use-list of a package
changes, which would occur if the |defpackage| form above were evaluated
after the tests have been loaded.

@l
(in-package "SLUDGE")
@e
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-rt)
  (do-external-symbols (symbol (find-package "SB-RT"))
    (import symbol)))

@ We'll define our global variables and condition classes as we need them,
but we'd like them to appear near the top of the tangled output.

@l
@<Global variables@>
@<Condition classes@>

@ A teeny-tiny server.

@l
(defgeneric server-listen (address &key backlog)
  (:method ((address pathname) &key (backlog 1))
    (ignore-errors (delete-file address))
    (let ((socket (make-instance 'local-socket :type :stream)))
      (socket-bind socket (namestring address))
      (socket-listen socket backlog)
      socket)))

(defun server-accept (socket &key (repl 'simple-repl) (spawn t))
  (let ((peer (socket-accept socket)))
    (if spawn
        (make-thread 'server-loop :arguments (list peer repl))
        (server-loop peer repl))))

(defun server-loop (client repl &key (external-format :default))
  (unwind-protect
       (let* ((stream (socket-make-stream client
                                          :input t :output t
                                          :buffering :none
                                          :external-format external-format))
              (*standard-input* stream)
              (*standard-output* stream))
         (funcall repl))
    (socket-close client)))

@ Here's a teeny-tiny little top-level \repl. It's mostly useful for testing
the server. Most of this implementation was cribbed from SBCL's \repl.

@l
(defun toplevel-eval (form &key (eval #'eval))
  "Evaluate FORM, returning whatever it returns and adjusting ***, **, *,
+++, ++, +, ///, //, /, and -."
  (setq - form)
  (unwind-protect
       (let ((values (multiple-value-list (funcall eval form))))
         (setq /// // // / / values
               *** ** ** * * (first values)))
    (setq +++ ++ ++ + + -))
  (values-list /))

(defun simple-repl (&key (prompt (lambda (stream) (format stream "~&* "))))
  (loop
    (with-simple-restart (abort "~@<Return to REPL.~@:>")
      (funcall prompt *standard-output*)
      (force-output)
      (let* ((form (handler-case (read) (end-of-file () (return))))
             (values (multiple-value-list (toplevel-eval form))))
        (if values
            (mapc (lambda (value) (format t "~S~&" value)) values)
            (fresh-line))))))

@ A \sludge\ server communicates with a client via a byte-oriented,
bi-directional communications protocol. This protocol may be informally
specified as follows. Sequences of octets (8-bit bytes) are to be
interpreted as representing Unicode characters using a pre-arranged
encoding. (Future versions of the protocol might have some kind of encoding
negotiation.) The characters form sexps, but with a highly restricted
syntax. The sexps denote {\it messages\/}, which are conceptually divided
into {\it requests\/} and {\it responses\/}, but share a common syntax.

Messages consist of lists of the form |(code tag args)|, where |code| is
any keyword symbol name valid in both Common and Emacs Lisp, |tag| is a
client-generated identifaction tag for this message, and |args| is a list
of symbols, strings, and lists of same. If |args| is null and the tag is
unimportant, the parentheses may be elided; thus |:code| is interpreted
as a designator for |(:code nil)|.

Requests are messages whose code is any keyword symbol except |:ok|
or~|:error|. The meaning and syntax of the arguments vary.

Responses always take the form |(status code tag args)|, where |status| is
either |:ok| or |:error|. The former are called {\it successful requests},
and the args that follow contain the results of the request. If processing
the request caused an error to be signaled, then an {\it error response\/}
is generated with the form |(:error code tag name message)|, where |name|
is the name of the error condition signaled and |message| is a string
containing any available information about the cause of the error. In both
response types, |code| and |tag| are the same as those of the request that
caused this response to be generated.

@l
(deftype code () 'keyword)
(deftype request-code () '(and code (not response-code)))
(deftype response-code () '(member :ok :error))

@t@l
(deftest code-types
  (values (typep :foo 'request-code)
          (typep :foo 'response-code)
          (typep :ok 'request-code)
          (typep :ok 'response-code)
          (typep 'foo 'request-code)
          (typep 'foo 'response-code))
  t nil nil t nil nil)

@ A request message is any message in which the code isn't a response code.

@l
(deftype request-message () '(cons request-code (cons * list)))

(defun make-request-message (code tag &rest args)
  (check-type code request-code)
  `(,code ,tag ,@args))

(defun message-code (message) (car message))
(defun message-tag (message) (cadr message))
(defun message-args (message) (cddr message))

@t@l
(deftest make-request-message
  (equal (make-request-message :foo nil 'bar 'baz) '(:foo nil bar baz))
  t)

(deftest request-message-type
  (values (typep '(:foo nil foo) 'request-message)
          (typep '(:ok nil foo) 'request-message)
          (typep '(:foo) 'request-message))
  t nil nil)

@ A response message begins with a response code followed by a request code.

@l
(deftype response-message () '(cons response-code (cons request-code list)))

(defun make-response-message (code request tag &rest args)
  (check-type code response-code)
  (check-type request request-code)
  `(,code ,request ,tag ,@args))

(defun response-code (message) (car message))
(defun request-code (message) (cadr message))
(defun response-tag (message) (caddr message))
(defun response-args (message) (cdddr message))

@t@l
(deftest response-message-type
  (values (typep '(:ok :foo nil 1 2 3) 'response-message)
          (typep '(:ok :ok nil 1 2 3) 'response-message)
          (typep '(:foo :foo nil 1 2 3) 'response-message))
  t nil nil)

@ We use the Lisp reader to pull messages off the wire.

@l
(defun read-message (&optional stream)
  (read stream))

@ So, having defined our protocol structures and types, let's turn to the
processing of requests. 

@l
(defun read-request (&optional stream)
  (let ((message (read-message stream)))
    (typecase message
      (request-code `(,message nil))
      (request-message message)
      (t (error 'invalid-request-message :message message)))))

@ @<Condition classes@>=
(define-condition invalid-request-message (error)
  ((message :reader invalid-request-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Malformed request ~S."
                     (invalid-request-message condition)))))

@t We'll frequently read requests from strings during tests, so we'll
define a little helper function for that.

@l
(defun read-request-from-string (string)
  (with-input-from-string (stream string)
    (read-request stream)))

@t@l
(deftest read-request
  (values (equal (read-request-from-string ":foo") '(:foo nil))
          (equal (read-request-from-string "(:foo nil)") '(:foo nil))
          (handler-case (read-request-from-string "(:ok)")
            (invalid-request-message () t)))
  t t t)


@ Sending messages is simple: we just print the character representation
to standard output, followed by a newline (for aesthetic purposes only).

@l
(defun send-message (message)
  (let ((*print-case* :downcase)
        (*print-readably* t))
    (write-string (format nil "~S~%" message))
    (finish-output)))

@ Our request handlers will be methods of the generic function
|handle-request|, specialized on request codes. To simplify things a bit,
we'll use an around~method that sends whatever message the applicable
primary method returns. If a method returns |nil|, nothing is sent to
the client.

@l
(defgeneric handle-request (code tag &rest args))

(defmethod handle-request :around (code tag &rest args)
  (declare (ignore code args))
  (let ((response (call-next-method)))
    (when response
      (assert (eql (response-tag response) tag) (response tag)
              "Message tag in response does not match request.")
      (send-message response))))

@ The default handler just signals an error.

@l
(defmethod handle-request (code tag &rest args)
  (error 'invalid-request-message :message `(,code ,tag ,@args)))

@ Here's a minimal request handler. It returns |nil|, so no response will
be sent.

@l
(defmethod handle-request ((code (eql :nop)) tag &rest args)
  (declare (ignore tag args)))

@ Here's a slightly more interesting request. It echoes its arguments.

@l
(defmethod handle-request ((code (eql :ping)) tag &rest args)
  (apply #'make-response-message :ok code tag args))

@ We'll use the full power of Common Lisp's lambda list to parse request
messages. The safety declaration is to ensure that |destructuring-bind|
signals an error if the request does not match the destructuring pattern.

@l
(defmacro destructure-request (lambda-list request &body body)
  `(destructuring-bind ,lambda-list ,request
     (declare (optimize safety))
     ,@body))

@ Here's the reason this whole system exists: the |:arglist| request returns
the lambda list of the indicated function.

@l
(defmethod handle-request ((code (eql :arglist)) tag &rest args)
  (destructure-request (function) args
    (let ((arglist (sb-introspect:function-lambda-list function)))
      (make-response-message :ok code tag function arglist))))

@ @l
(defun sludge-loop ()
  (let ((*read-eval* nil)
        (*standard-input* (if *message-log*
                              (make-echo-stream *standard-input* *message-log*)
                              *standard-input*))
        (*standard-output* (if *message-log*
                               (make-broadcast-stream *standard-output* ;
                                                      *message-log*)
                               *standard-output*)))
    (loop
      (with-simple-restart (continue "Ignore this request.")
        (let ((request (handler-case (read-request)
                         (end-of-file () (return)))))
          (destructure-request (code tag &rest args) request
            (handler-case (apply #'handle-request code tag args)
              (error (condition)
                (send-message
                 (make-response-message :error code tag (type-of condition)
                                        (princ-to-string condition)))))))))))

@ To enable message tracing, one might set this variable to a synonym
stream for |*trace-output*|.

@<Global variables@>=
(defvar *message-log* nil
  "The stream to which SLUDGE messages should be logged, or NIL to disable
logging.")
