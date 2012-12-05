% -*- mode: CLWEB; package: SLUDGE; -*-
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SB-BSD-SOCKETS")
  (require "SB-POSIX"))
@e
(defpackage "SLUDGE"
  (:use "COMMON-LISP" "SB-BSD-SOCKETS" "SB-THREAD")
  (:import-from "SB-INTROSPECT" "FUNCTION-LAMBDA-LIST")
  (:import-from "SB-POSIX" "MKTEMP" "UMASK")
  (:export "START-SERVER" "STOP-SERVER"))
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
is specific to the |sb-bsd-sockets| module, but should be easily portable
to any standard {\sc bsd}-style socket {\sc api}.

When we make a socket for the server, we'll bind it to an address and have
it listen there for connections. The function |make-server-socket| thus
returns a new socket ready to accept connections, or else signals an error.
The domain of the constructed socket is determined automatically from the
type of address given, with slightly hairy defaulting behavior.

@l
@<Define server logging routine@>
@<Define |with-umask| macro@>

(deftype inet-addr () '(or (simple-vector 4) (vector (unsigned-byte 8) 4)))
(deftype inet-sock-addr () '(cons inet-addr (cons integer null)))

(defun make-server-socket (address &key (backlog *default-backlog*)
                           (port *default-port*) ;
                           (protocol :tcp) ;
                           (umask #O77) &aux
                           (address @<Resolve |address|...@>))
  (let ((socket (etypecase address
                  (inet-sock-addr (make-instance 'inet-socket ;
                                                 :type :stream ;
                                                 :protocol protocol))
                  (pathname (make-instance 'local-socket :type :stream)))))
    @<Bind |socket| to |address| and listen for connections@>
    (assert (socket-open-p socket))
    (server-log "Server listening on ~{~A~^ port ~D~}.~%"
                (multiple-value-list (socket-name socket)))
    socket))

@ In SBCL's socket library, local (Unix) domain socket addresses are
represented by namestrings, and {\sc inet} domain socket addresses are
represented by lists of the form |(ipv4-address port)|. In an effort
to be both slightly more accommodating to the user and more Lisp-like,
we accept a slightly different set of socket address designators, which
are easier to express in Lisp than prose. Note the defaulting for address
and port, and that strings are ambiguous: we first try to parse them as
dotted-quads, and only if that fails do we treat them as namestrings.

@<Resolve |address| as a socket address designator@>=
(etypecase address
  (null (list *localhost* port))
  (inet-addr (list address port))
  (inet-sock-addr address)
  (pathname address)
  (string (or (ignore-errors (list (make-inet-address address) port))
              (pathname address))))

@t We'll test both namestrings and pathnames as designators for local
domain socket addresses. We assume that {\tt /tmp} is an acceptable place
to put temporary socket files.

@l
(defun make-temp-socket-name ()
  (mktemp (make-pathname :name "socket"
                         :type "XXXXXX"
                         :directory '(:absolute "tmp"))))

(defmacro with-open-server-socket ((socket address &optional args) &body body)
  `(let ((,socket (apply #'make-server-socket ,address ,args)))
     (unwind-protect (progn ,@body)
       (socket-close ,socket))))

(defun test-make-local-server-socket (address)
  (with-open-server-socket (socket address)
    (unwind-protect
         (and (socket-open-p socket)
              (string= (socket-name socket) (namestring address))
              (probe-file address)
              t)
      (delete-file address))))

(deftest (make-server-socket local)
  (values (test-make-local-server-socket (make-temp-socket-name))
          (test-make-local-server-socket (pathname (make-temp-socket-name))))
  t t)

@t We have three kinds of address specifications to test for {\sc inet}
sockets: dotted quads, host \& port, and~(\<host> \<port>). We also verify
that an attempt to bind to the same socket address while a socket is still
open signals an address-in-use error.

We assume that |*default-port*| on the local host is available for binding.

@l
(defun test-make-inet-server-socket (address &rest args)
  (with-open-server-socket (socket address args)
    (and (socket-open-p socket)
         (typep (multiple-value-list (socket-name socket)) 'inet-sock-addr)
         (handler-case
             (apply #'make-server-socket address args)
           (address-in-use-error () t)))))

(deftest (make-server-socket inet)
  (values (test-make-inet-server-socket "127.0.0.1")
          (test-make-inet-server-socket *localhost* :port *default-port*)
          (test-make-inet-server-socket (list *localhost* *default-port*)))
  t t t)

@ If we're binding to an {\sc inet} socket, we'll use the loopback address
and a pseudo-random (but fixed) unprivledged port as defaults.

@<Global variables@>=
(defparameter *localhost* #(127 0 0 1)
  "The loopback address.")

(defparameter *default-port* 31415
  "Default port on which to listen for connections.")

@ The |backlog| parameter to |socket-listen| controls the maximum queue
length for new connections: if there are more than this many outstanding
connection requests, new connection attemps will be refused. See \man
listen(2) for more information. A value of~5 is traditional, and since
this server is expected to handle only low traffic volume, it should be
more than sufficient.

@<Global variables@>=
(defparameter *default-backlog* 5
  "Maximum length of pending connections queue.")

@ Both binding a socket to an address and attempting to listen on it can
fail for many reasons. If they do, there's not much that can be usefully
done with the socket, so we'll offer an |abort| restart that shuts it down
and returns |nil|.

@<Bind |socket| to |address| and listen for connections@>=
(restart-case (progn
                @<Bind |socket| to |address|@>
                (socket-listen socket backlog))
  (abort ()
    :report "Abort and close the socket."
    (socket-close socket)
    (abort)))

@ When attempting to bind a socket to an address, though, there's the
possibility of transient failure; e.g., another server might already
be bound to that address. So we'll provide a |retry| restart to allow
the user the possibility of correcting the error (e.g., shutting down
the other server).

When binding to a local domain socket, we'll first remove any file that
already exists at that address, then lock down permissions on the socket
file that we create by temporarily changing the file creation mode mask;
see \man umask(2).

@<Bind |socket| to |address|@>=
(tagbody
 bind
  (restart-case
      (etypecase address
        (inet-sock-addr (apply #'socket-bind socket address))
        (pathname (ignore-errors (delete-file address))
                  (with-umask umask
                    (socket-bind socket (namestring address)))))
    (retry ()
      :report "Retry binding the socket."
      (go bind))))

@t@l
(deftest (make-server-socket retry)
  (let ((a (make-server-socket nil)))
    (when (socket-open-p a)
      (let ((b (handler-bind ((address-in-use-error
                               (lambda (condition)
                                 (declare (ignore condition))
                                 (socket-close a)
                                 (invoke-restart 'retry))))
                 (make-server-socket nil))))
        (unwind-protect (socket-open-p b)
          (socket-close b)))))
  t)

@ @<Define |with-umask|...@>=
(defmacro with-umask (umask &body body)
  (let ((old-umask (make-symbol "OLD-UMASK")))
    `(let ((,old-umask (umask ,umask)))
       (unwind-protect (progn ,@body)
         (umask ,old-umask)))))

@ Once a socket is bound and listening, it is ready to accept connections.
As soon as we accept a connection, we'll enter what we call, for lack of
a better term, a \repl: a loop, implemented as a function of no arguments,
that reads messages from standard input and responds on standard output.
Error output is {\it not\/} rebound, so that the \repl\ has a stream on
which to write error messages that might reach the user directly.

By default, we'll spawn a new thread for each \repl\ so that it can operate
in the background. During debugging, however, it can be useful to run in
the foreground, so we'll support a |spawn| keyword argument which can be
used to override the default behavior.

@l
(defun serve-client (client repl &key (external-format :default))
  (unwind-protect
       (let* ((stream (socket-make-stream client
                                          :input t :output t
                                          :buffering :none
                                          :external-format external-format))
              (*standard-input* stream)
              (*standard-output* stream))
         (server-log "Entering ~A.~%"
                     (etypecase repl
                       (symbol repl)
                       (function (nth-value 2 ; name
                                            (function-lambda-expression ;
                                             repl)))))
         (funcall repl))
    (socket-close client)
    (server-log "Client connection closed.~%")))

(defun server-accept (socket repl &key (spawn t))
  (let ((client (socket-accept socket)))
    (server-log "Accepted connection on ~{~A~^ port ~D~}~
                 ~{ from client at ~A~^ port ~D~}.~%"
                (multiple-value-list (socket-name socket))
                (multiple-value-list (socket-peername client)))
    (if spawn
        (make-thread 'serve-client ;
                     :arguments (list client repl) ;
                     :name "SLUDGE client")
        (serve-client client repl))))

(defun server-loop (server repl &key (spawn t) once-only)
  (unwind-protect
       (loop
         (server-accept server repl :spawn spawn)
         (when once-only (return)))
    (when (typep server 'local-socket)
      (ignore-errors (delete-file (socket-name server))))
    (socket-close server)
    (server-log "Server socket closed.~%")))

@ With the above machinery in place, we come now to the primary public
interface of the whole system: a pair of functions which start and stop,
respectively, a server loop thread. If the |spawn| argument to |start-server|
is true, however, the server loop will run in the current thread; this is
for debugging purposes only. Note that running client threads are currently
{\it not\/} aborted when the server is stopped; only the server loop itself
is halted, so no new client connections will be accepted.

@l
(defun start-server (&rest args &key address (repl 'main-loop) (spawn t) ;
                     once-only &allow-other-keys)
  (let ((server (apply #'make-server-socket address :allow-other-keys t args)))
    (flet ((serve ()
             (server-loop server repl :spawn spawn :once-only once-only)))
      (if spawn
          (setq *server* (make-thread #'serve :name "SLUDGE server"))
          (serve)))))

@ @<Global variables@>=
(defvar *server* nil
  "The current SLUDGE server.")

@ This somewhat violent and crude implementation of |stop-server| at least
has the advantage of simplicity. Less harsh solutions that do not add undue
complexity would be welcome.

@l
(defun stop-server (&optional (server *server*))
  (interrupt-thread server #'abort))

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
  (with-standard-io-syntax
    (loop
      (with-simple-restart (abort "~@<Return to REPL.~@:>")
        (funcall prompt *standard-output*)
        (force-output)
        (let* ((form (handler-case (read) (end-of-file () (return))))
               (values (multiple-value-list (toplevel-eval form))))
          (if values
              (mapc (lambda (value) (format t "~S~&" value)) values)
              (fresh-line)))))))

@ What's a server without logging? The server functions above all send
debugging messages to the stream in |*log-output*| using this function.

@<Define server logging routine@>=
(defun server-log (control-string &rest args)
  "Send a formatted debugging message to the log output stream."
  (when *log-output*
    (apply #'format *log-output* control-string args)
    (force-output *log-output*)))

@ A few potentially useful values for |*log-output|: |nil|, meaning no
messages will be logged; a synonym stream for |*terminal-io*|; or~a file
output stream.

@<Global variables@>=
(defvar *log-output* nil
  "A designator for a stream on which to print server debugging messages.")

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

@ Sending messages is simple: we just print the character representation
to standard output, followed by a newline (for aesthetic purposes only).

@l
(defun send-message (message)
  (let ((*print-case* :downcase)
        (*print-pretty* nil))
    (write-string (format nil "~S~%" message))
    (finish-output)))

@ Here's a little convenience method for sending error messages.

@l
(defun send-error-message (code tag condition)
  (send-message
   (make-response-message :error code tag
                          (type-of condition)
                          (princ-to-string condition))))

@ To pull a request off the wire, we'll use the Lisp reader, but in a very
careful way. We start by collecting the characters that comprise the
message using the Lisp reader with |*read-suppress*| bound to true; that
should catch the most basix syntax errors. Then we'll peek at the first
character so we can handle the abbreviated request syntax described above.
If it's a fully parenthesized message, we try to read the code and tag of
the message---if we can't get those, then we can't even send a proper error
response---followed by the arguments.

This routine will signal errors if it detects any abnormalities in the
syntax of the message; it is expected that higher-level routines will
establish handlers that can cope with such errors in a sensible way (e.g.,
by ignoring the whole message).

@l
(defun read-request (&optional (stream *standard-input*))
  (with-input-from-string ;
      (*standard-input* @<Collect the characters that comprise this message@>)
    (macrolet ((read-typed-object (type)
                 (let ((object (gensym)))
                   `(let ((,object (read)))
                      (check-type ,object ,type)
                      ,object))))
      (let ((c (peek-char t)))
        (case c
          (#\: (list (read-typed-object request-code) 0))
          (#\( (read-char)
               (let* ((code (read-typed-object request-code))
                      (tag (read-typed-object tag))
                      (args @<Try to read arguments until closing paren@>))
                 (list* code tag args))))))))

@ @<Collect the characters...@>=
(with-output-to-string (string-stream)
  (let ((*standard-input* (make-echo-stream (or stream *standard-input*) ;
                                            string-stream))
        (*read-suppress* t))
    (read)))

@ Arguments are read one at at a time until we see the closing delimiter.
If an error is signaled while attempting to read an argument, we'll send
back an error response, but nevertheless decline to handle the condition.

@<Try to read arguments...@>=
(loop until (char= (peek-char t) #\))
      collect (handler-bind ;
                  ((reader-error (lambda (condition)
                                   (send-error-message code tag condition))))
                (read)))

@ SBCL's reporting functions for reader errors can themselves signal
errors, even for offences as minor as attempting to report an error about
a stream that has since been closed. Whence the careful error handling
in our own report function, which implicitly relies on theirs.

@<Condition classes@>=
(define-condition unreadable-object (error)
  ((error :reader unreadable-object-error :initarg :error))
  (:report (lambda (condition stream)
             (or (ignore-errors
                   (princ (unreadable-object-error condition) stream))
                 (format stream "Error attempting to read object.")))))

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
          (null (read-request-from-string "<invalid>")))
  t t t)

@ Request handlers are methods of the generic function |handle-request|,
specialized on request codes.

@l
(defgeneric handle-request (code tag &rest args))

@ The default handler just signals an error.

@l
(defmethod handle-request (code tag &rest args)
  (error 'invalid-request-message :message `(,code ,tag ,@args)))

@ @<Condition classes@>=
(define-condition invalid-request-message (error)
  ((message :reader invalid-request-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Invalid request ~S."
                     (invalid-request-message condition)))))

@ Request handlers tend to follow a similar pattern, so we'll use a
defining macro that abstracts it a bit. The request arguments are bound
using |destructuring-bind| to the parameters specified by |lambda-list|,
and the body should return a designator for a list of response arguments,
from which a response message will be constructed and sent. We'll leave
it up to the main loop to handle any errors by constructing and sending
error responses.

@l
(defmacro define-request-handler (request-code lambda-list &body body)
  (let ((code (make-symbol "CODE"))
        (tag (make-symbol "TAG"))
        (args (make-symbol "ARGS"))
        (response-args (make-symbol "RESPONSE-ARGS")))
   `(defmethod handle-request ((,code (eql ,request-code)) ,tag &rest ,args)
      (declare (optimize safety))
      (destructuring-bind ,lambda-list ,args
        (let ((,response-args (progn ,@body)))
          (send-message (apply #'make-response-message :ok ,code ,tag
                               (if (listp ,response-args)
                                   ,response-args
                                   (list ,response-args)))))))))

@ Here's a minimal request handler: it simply echoes its arguments.

@l
(define-request-handler :echo (&rest args) args)

@ And here's the main reason this whole system exists: the |:arglist|
request returns the lambda list of the indicated function.

@l
(define-request-handler :arglist (function)
  (list function (function-lambda-list function)))

@ We might want documentation strings as well.

@l
(define-request-handler :documentation (x doc-type)
  (list (documentation x doc-type)))

@ This next command allows the client to set the current package. In order
to correctly obtain information about objects named by non-package-prefixed
symbols, the current package must match the implicit package of the given
symbol.

@l
(define-request-handler :in-package (name)
  (let ((package (find-package name)))
    (if package
        (progn (setq *package* package)
               (package-name package))
        (error 'no-such-package-error :package name))))

@ @<Condition classes@>=
(define-condition no-such-package-error (package-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No such package: ~A" ;
                     (package-error-package condition)))))

@ Clients are free to just disconnect when they're done, but they might
prefer to be polite about it and ask that the connection be terminated.

@l
(define-request-handler :bye (&rest args)
  (signal 'client-disconnect :args args))

@ @<Condition classes@>=
(define-condition client-disconnect ()
  ((args :reader client-disconnect-args :initarg :args)))

@ Having defined all of our message handlers, let's return to the server
proper. Our main loop for the \sludge\ server reads a request, calls
|handle-request|, then loops. If \eof\ is encountered during the read, we
exit the loop. If an error occurs during request handling, we send an error
response and continue processing with the next message. If the client
signals their desire to disconnect, we acknowledge the request and exit
the loop. We also establish a |continue| restart which simply ignores the
current request and picks up with the next one.

@l
(defun main-loop ()
  (let ((*read-eval* nil)
        (*standard-input* @<Maybe echo standard input to the message log@>)
        (*standard-output* @<Maybe echo standard output to the message log@>))
    (loop
      (with-simple-restart (continue "Ignore this request.")
        (let ((request (handler-case (read-request)
                         (reader-error () (continue))
                         (type-error () (continue))
                         (invalid-request-message () (continue))
                         (end-of-file () (return)))))
          (destructuring-bind (code tag &rest args) request
            (handler-case (apply #'handle-request code tag args)
              (client-disconnect (condition)
                (send-message ;
                 (apply #'make-response-message :ok code tag
                        (client-disconnect-args condition)))
                (return))
              (error (condition)
                (send-error-message code tag condition)))))))))

@ If the variable |*message-log*| is set to an output stream, we'll arrange
for everything read from standard input and written to standard output to
be echoed to that stream. A synonym stream for |*trace-output*| or an output
file stream would both be useful values here.

@<Global variables@>=
(defvar *message-log* nil
  "The stream to which SLUDGE messages should be logged.")

@ @<Maybe echo standard input...@>=
(if (and *message-log* (output-stream-p *message-log*))
    (make-echo-stream *standard-input* *message-log*)
    *standard-input*)

@ @<Maybe echo standard output...@>=
(if (and *message-log* (output-stream-p *message-log*))
    (make-broadcast-stream *standard-output* *message-log*)
    *standard-output*)

@*Index.
@t*Index.
