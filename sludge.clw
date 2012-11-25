\font\sc=cmcsc10
\def\<#1>{\leavevmode\hbox{$\mkern-2mu\langle${\it #1\/}$\rangle$}}
\def\etc.{{\it \char`&c.\spacefactor1000}}
\def\eof{{\sc eof}}
\def\repl{{\sc repl}}
\def\sludge{{\sc sludge}}
\def\man#1(#2){the Unix manual page {\bf #1(#2)}}

@*\sludge. The Simple Lisp Usage and Documentation Gathering Engine allows
Emacs (or any other interested client) to request various pieces of
information from a running Common Lisp process: e.g., function argument
lists, documentation strings, possible symbol completions, \etc. A server
written in Common Lisp (this file) listens for connections from a client
written in Emacs Lisp ({\tt sludge.el}), then reads and responds to
requests. The system thus behaves essentially as a remote procedure call
mechanism, with Emacs calling down to a set of pre-defined procedures on
the Lisp side. The protocol that governs the communication is a simple
sexp-based stream of Unicode characters, encoded as a stream of octets,
and may utilize any of several suitable network transports.

The server currently runs only on multi-threaded builds of SBCL, but should
be simple enough to port to other implementations.

@l
(provide "SLUDGE")
@e
(defpackage "SLUDGE"
  (:use "COMMON-LISP" "SB-BSD-SOCKETS" "SB-THREAD")
  (:import-from "SB-POSIX" "UMASK")
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

@ We'll start with the low-level guts of the server. This implementation
is specific to the |sb-sockets| module, but should be easily portable to
any standard {\sc bsd}-style socket {\sc api}.

To set up the server, we create a socket, bind it to an address, and listen
for connections. Both {\sc inet} and Unix domain sockets are supported.
{\sc inet} addresses are denoted by vectors of |(unsigned-byte 8)| or
lists of the form |(address port)|, while Unix domain socket addresses
are denoted by pathnames.

@l
@<Define |with-umask| macro@>

(defgeneric server-listen (address &key)
  (:method ((address pathname) &key (umask #O77))
    (ignore-errors (delete-file address))
    (let ((socket (make-instance 'local-socket :type :stream)))
      (with-umask umask
        (socket-bind socket (namestring address)))
      socket))
  (:method ((address vector) &key (port *default-port*))
    (server-listen (list address port)))
  (:method ((address null) &key (port *default-port*))
    (server-listen (list *localhost* port)))
  (:method ((address cons) &key (protocol :tcp))
    (let ((socket (make-instance 'inet-socket ;
                                 :type :stream ;
                                 :protocol protocol)))
      (destructuring-bind (address port) address
        (socket-bind socket address port))
      socket))
  (:method :around (address &key (backlog *default-backlog*) (verbose nil))
    (let ((socket (call-next-method)))
      (when socket
        (socket-listen socket backlog)
        (when verbose
          (multiple-value-bind (address port) (socket-name socket)
            (format t "Server listening on ~A~@[ port ~D~].~%" address port))))
      socket)))

@ This little macro executes its body with a temporary file creation mode
mask; see \man umask(2). We use it to lock down permissions on a Unix domain
socket.

@<Define |with-umask|...@>=
(defmacro with-umask (umask &body body)
  (let ((old-umask (make-symbol "OLD-UMASK")))
    `(let ((,old-umask (umask ,umask)))
       (unwind-protect (progn ,@body)
         (umask ,old-umask)))))

@ If we're binding to an {\sc inet} socket, we'll use the loopback address
and an unprivledged port by default.

@<Global variables@>=
(defparameter *default-port* 31415
  "Default INET port on which to listen for connections.")

(defparameter *localhost* #(127 0 0 1)
  "The loopback address.")

@ The |backlog| parameter to |socket-listen| controls the maximum queue
length for new connections: if there are more than this many outstanding
connection requests, new connection attemps will be refused.
See \man listen(2) for more information.

@<Global variables@>=
(defparameter *default-backlog* 10
  "Maximum length of pending connections queue.")

@ Once a socket is bound and listening, it is ready to accept connections.
As soon as we accept a connection, we'll enter the main server loop, which
is implemented by a function of no arguments that reads messages from
standard input and responds on standard output. Error output is {\it not\/}
rebound, so that server loops have a stream on which to write error messages
that might reach the user directly.

By default, we'll spawn a new thread for each server loop so that it can
operate in the background. During debugging, however, it can be useful to
run the server loop in the foreground, so we'll support a |spawn| keyword
argument which can be used to override the default behavior.

@l
(defun server-accept (socket server-loop &key (spawn t) (verbose nil))
  (let ((client (socket-accept socket)))
    (when verbose
      (format t "Accepted connection on ~A~@[ from client ~A~].~%"
              (socket-name socket) (socket-peername client)))
    (if spawn
        (make-thread 'serve-client :arguments (list client server-loop))
        (serve-client client server-loop))))

(defun serve-client (client server-loop &key (external-format :default))
  (unwind-protect
       (let* ((stream (socket-make-stream client
                                          :input t :output t
                                          :buffering :none
                                          :external-format external-format))
              (*standard-input* stream)
              (*standard-output* stream))
         (funcall server-loop))
    (socket-close client)))

@t Here's a teeny-tiny little top-level \repl, just for testing the server.
Most of this implementation was cribbed from SBCL's \repl.

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

(defun serve-repl (&key (address #P"/tmp/repl") (spawn nil) (verbose t))
  (let ((server (server-listen address :verbose verbose)))
    (unwind-protect
         (server-accept server 'simple-repl ;
                        :spawn spawn ;
                        :verbose verbose)
      (socket-close server)
      (when verbose (format t "Connection closed.~%"))
      (when (pathnamep address)
        (delete-file address)))))

@ The \sludge\ protocol may be informally specified as follows. Sequences
of octets (8-bit bytes) are interpreted as representing Unicode characters
using a pre-arranged encoding. (Future versions of the protocol might have
some kind of encoding negotiation.) The characters form sexps, but with a
highly restricted syntax. The sexps denote {\it messages\/}, which are
divided into {\it requests\/} and {\it responses\/}.

Requests are represented as lists of the form |(code tag args)|, where
|code| is any keyword symbol other than |:ok| and~|:error|, |tag| is a
client-supplied integer identifier for this message, and |args| is an
arbitrary list of of Lisp objects. If |args| is null and the tag is not
important, the parentheses may be elided; thus |:code| is interpreted as a
designator for |(:code 0)|.

Responses take the form |(response-code request-code tag args)|, where
|response-code| is either |:ok| or~|:error|, and the |request-code| and
|tag| are taken from the request to which this message is a response.
A successful request generates a response whose status is |:ok|, and the
args that follow contain the results of the request. If processing the
request caused an error to be signaled, then an {\it error response\/}
is generated with the form |(:error code tag name message)|, where |name|
is the name of the error condition signaled and |message| is a string
containing any available information about the cause of the error.

@l
(deftype code () 'keyword)
(deftype tag () 'integer)
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
(deftype request-message () '(cons request-code (cons tag list)))

(defun make-request-message (code tag &rest args)
  (declare (request-code code)
           (tag tag))
  `(,code ,tag ,@args))

(defun request-code (message) (car message))
(defun request-tag (message) (cadr message))
(defun request-args (message) (cddr message))

@t@l
(deftest make-request-message
  (equal (make-request-message :foo 0 'bar 'baz) '(:foo 0 bar baz))
  t)

(deftest request-message-type
  (values (typep '(:foo 0 foo) 'request-message)
          (typep '(:ok 0 foo) 'request-message)
          (typep '(:foo) 'request-message))
  t nil nil)

@ A response message begins with a response code, which is followed by what
looks like a request message.

@l
(deftype response-message () '(cons response-code request-message))

(defun make-response-message (response-code request-code tag &rest args)
  (declare (response-code response-code)
           (request-code request-code)
           (tag tag))
  `(,response-code ,request-code ,tag ,@args))

(defun response-code (message) (car message))
(defun response-request-code (message) (cadr message))
(defun response-tag (message) (caddr message))
(defun response-args (message) (cdddr message))

@t@l
(deftest make-response-message
  (equal (make-response-message :ok :foo 0 'bar 'baz) '(:ok :foo 0 bar baz))
  t)

(deftest response-message-type
  (values (typep '(:ok :foo 0 t nil t) 'response-message)
          (typep '(:ok :ok 0 t nil t) 'response-message)
          (typep '(:foo :foo 0 t nil t) 'response-message))
  t nil nil)

@ We use the Lisp reader to pull messages off the wire.

@l
(defun read-message (&optional stream)
  (read stream))

@ On the server side, we'll only ever read request messages. This function
reads one request off the wire and verifies the basic structure of the
message.

@l
(defun read-request (&optional stream)
  (let ((message (read-message stream)))
    (typecase message
      (request-code `(,message 0))
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
  (values (equal (read-request-from-string ":foo") '(:foo 0))
          (equal (read-request-from-string "(:foo 0)") '(:foo 0))
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

@*Index.
@t*Index.
