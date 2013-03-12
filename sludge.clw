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

@1*Serving network clients. We'll start with the low-level guts of the
server. This implementation is specific to the |sb-bsd-sockets| module, but
should be easily portable to any standard {\sc bsd}-style socket {\sc api}.

@ What's a server without logging? We'll use a subclass of |warning| so that
they can be easily muffled.

@l
(defun server-log (format-control &rest args)
  (warn 'server-log :format-control format-control :format-arguments args))

@ @<Condition classes@>=
(define-condition server-log (simple-warning) ())

@ When we make a socket for the server, we'll bind it to an address and have
it listen there for connections. The function |make-server-socket| thus
returns a new socket ready to accept connections, or else signals an error.
The domain of the constructed socket is determined automatically from the
type of address given, with slightly hairy defaulting behavior.

@l
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
    (server-log "Server listening on ~{~A~^ port ~D~}."
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
  `(let ((,socket
          (handler-bind ((server-log #'muffle-warning))
            (apply #'make-server-socket ,address ,args))))
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
             (handler-bind ((server-log #'muffle-warning))
               (apply #'make-server-socket address args))
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
fail for many reasons. If the listen fails, there's usually not much that
can be usefully done with the socket, so we'll offer an |abort| restart
that closes it down gracefully.

@<Bind |socket| to |address| and listen for connections@>=
(restart-case (progn
                @<Bind |socket| to |address|@>
                (socket-listen socket backlog))
  (abort ()
    :report "Abort and close the socket."
    (socket-close socket)
    (abort)))

@ But with binding, transient failures are more common, so we'll provide a
|retry| restart to allow the user the possibility of correcting the error.
The most common cause of transient binding failure is probably the address
being already in use (e.g., by another server).

When binding to a local domain socket, we'll also give the option to remove
any file that already exists at that address.

@<Bind |socket| to |address|@>=
(tagbody
 bind
  (restart-case
      (etypecase address
        (inet-sock-addr (apply #'socket-bind socket address))
        (pathname (with-umask umask
                    (restart-case (socket-bind socket (namestring address))
                      (delete-file ()
                        :report "Delete the socket file and retry binding."
                        (ignore-errors (delete-file address))
                        (go bind))))))
    (retry ()
      :report "Retry binding the socket."
      (go bind))))

@t First, we'll open a local socket |a| at some address (filename). Then
we'll try to open a socket~|b| at the same address; this will fail, since
the address is already in use (by~|a|). So we close |a| and invoke the
|delete-file| restart; |b| should then open.

@l
(deftest (make-server-socket retry)
  (let ((address (make-temp-socket-name)))
    (with-open-server-socket (a address)
      (let* ((retried nil)
             (b (handler-bind ((address-in-use-error
                                (lambda (condition)
                                  (declare (ignore condition))
                                  (setq retried t)
                                  (invoke-restart 'delete-file)))
                               (server-log #'muffle-warning))
                  (make-server-socket address))))
        (unwind-protect (values (socket-open-p a) (socket-open-p b) retried)
          (socket-close b)
          (delete-file address)))))
  t t t)

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
         (server-log "Entering ~A."
                     (etypecase repl
                       (symbol repl)
                       (function (nth-value 2 ; name
                                            (function-lambda-expression ;
                                             repl)))))
         (funcall repl))
    (prog1 (socket-close client)
      (server-log "Client connection closed."))))

(defun server-accept (socket repl &key (spawn t))
  (let ((client (socket-accept socket)))
    (server-log "Accepted connection on ~{~A~^ port ~D~}~
                 ~{ from ~:[local client~;client at ~:*~A~^ port ~D~]~}."
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
    (prog1 (socket-close server)
      (server-log "Server socket closed."))))

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
  @<Offer to shut down an already-running server@>
  (let ((server (apply #'make-server-socket address :allow-other-keys t args)))
    (flet ((serve () ;
             (server-loop server repl :spawn spawn :once-only once-only)))
      (if spawn
          (setq *server* (make-thread #'serve :name "SLUDGE server"))
          (let ((*server* *current-thread*))
            (serve))))))

@ @<Global variables@>=
(defvar *server* nil
  "The current SLUDGE server.")

@ One could run more than one instance of the server per Lisp process, but
there's no particular reason to do so, and it isn't expected to be a common
practice.

@<Offer to shut down...@>=
(when (and spawn *server* (ignore-errors (thread-alive-p *server*)))
  (restart-case (cerror "Ignore it." 'server-already-running :thread *server*)
    (shutdown (&optional (server *server*))
      :report "Shut it down."
      (stop-server server))))

@ @<Condition classes@>=
(define-condition server-already-running ()
  ((thread :reader server-thread :initarg :thread))
  (:report (lambda (condition stream)
             (format stream "A server is already running: ~A." ;
                     (server-thread condition)))))

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
  "Evaluate FORM, returning whatever it returns and adjusting the
variables ***, **, *, +++, ++, +, ///, //, /, and -."
  (setq - form)
  (unwind-protect
       (let ((values (multiple-value-list (funcall eval form))))
         (shiftf /// // / values)
         (shiftf *** ** ** * (first values)))
    (shiftf +++ ++ + -))
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

@ Since we haven't yet even described our protocol, we can only sketch
our main loop at this time. We'll fill in the details later, after we've
defined our protocol interface.

@l
(defun main-loop ()
  (let ((*read-eval* nil))
    (loop @<Read and handle a request@>)))

@1*Protocol definition. The \sludge\ protocol may be informally described
as follows. Sequences of octets (8-bit bytes) are interpreted as representing
Unicode characters using a pre-arranged encoding. (Future versions of the
protocol might have some kind of encoding negotiation.) The characters form
s-expressions, but with a highly restricted syntax. The s-exps denote
{\it messages\/},which are divided into {\it requests\/} and {\it responses\/}.

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
should catch the most basic syntax errors. Then we'll peek at the first
character so we can handle the abbreviated request syntax described above.
If it's a fully parenthesized message, we try to read the code and tag of
the message---if we can't get those, then we can't even send a proper error
response---followed by the arguments.

This routine will signal errors if it detects any abnormalities in the
syntax of the message; it is expected that higher-level routines will
establish handlers that can cope with such errors in a sensible way (e.g.,
by ignoring the whole message).

@l
(defun read-request (&optional (stream *standard-input*) &aux ;
                     (*readtable* *request-readtable*))
  (with-input-from-string
      (*standard-input*
       (with-output-to-string (string-stream)
         (let ((*standard-input* (make-echo-stream stream string-stream))
               (*read-suppress* t))
           (read))))
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

@ Arguments are read one at at a time until we see the closing delimiter.
If an error is signaled while attempting to read an argument, we'll send
back an error response, but nevertheless decline to handle the condition.

@<Try to read arguments...@>=
(flet ((send-error-response (condition)
         (send-error-message code tag condition)))
  (loop until (char= (peek-char t) #\))
        collect (handler-bind ((reader-error #'send-error-response))
                  (read))))

@ Two of the main reasons for this system's existence are arglist display
and symbol completion. We'll come to the actual handling of these shortly,
but for now we'll stick to what's required to read the arguments to such
requests. The issue is that in both cases, there's a chance that the
primary argument will not be the name of any interned symbol. (The
probability is low to moderate in the first case; e.g., the user may have
paused in typing the name of a symbol when the arglist request is issued.
But there's a very high probability indeed in the second case---otherwise,
what's the point of completion?) If we were to just use |read| on such an
incomplete symbol name, the effect would be to (1)~intern a symbol that we
probably don't want, and (2)~lose information about whether and what kind
of a package prefix was specified. Neither is desirable in this
application.

Our workaround is to repurpose the quote mark as a `raw symbol' marker:
the token following the \.{'} is read and parsed according to the normal
rules, but is not immediately interned. Instead, an instance of |raw-symbol|
is returned, which preserves all of the information about the symbol normally
discarded by the reader.

We can think of the raw symbol structures as consisting primarily of two
sets of slots, each representing a different aspect of the raw symbol.
The first is the `parsed' version: normalized (i.e., without escapes),
case-folded package and symbol names with a boolean flag denoting internal
access. The second is a `split' version of the original token, consisting
of the raw package prefix, markers, and symbol name. We'll have occasion
to use both sets, especially when we get to symbol completion.

The last slot, |print-case|, is used to help transform the case of symbol
names back to the case given on input.

@l
(defstruct raw-symbol
  package internal name ; parsed
  prefix markers suffix ; split
  (print-case :downcase))

@<Define token reading routines@>
@<Define symbol parsing routines@>

(defun read-raw-symbol (stream char)
  (declare (ignore char))
  (parse-symbol (read-token stream)))

(set-macro-character #\' #'read-raw-symbol nil *request-readtable*)

@ @<Glob...@>=
(defvar *request-readtable* (copy-readtable nil))

@ If Common Lisp provided a |read-token| function, programs that need to
analyze Lisp source code would be much easier to write than they currently
are. Unfortunately, it doesn't, so we'll need to roll our own. We'll start
with a few helper routines.

@ The predicate |whitespacep| determines whether or not a given character
should be treated as whitespace. Note, however, that this routine does
not---and can not, at least not portably---examine the current readtable
to determine which characters currently have ${\it whitespace}_2$ syntax.

@<Define token reading...@>=
(defun whitespacep (char)
  (find char *whitespace* :test #'char=))

@ Only the characters named `Newline' and `Space' are required to be
present in a conforming Common Lisp implementation, but most also
support the semi-standard names `Tab', `Linefeed', `Return', and~`Page'.
Any of these that are supported should be considered whitespace characters.

@<Glob...@>=
(defparameter *whitespace*
  (coerce (remove-duplicates
           (remove nil (mapcar #'name-char ;
                               '("Newline" "Space" "Tab" ;
                                 "Linefeed" "Return" "Page"))))
          'string))

@ Our strategy for reading a token is to let the Lisp reader do the bulk
of the work by calling |read| with |*read-suppress*| bound to true, and
to catch the characters that it picks up using an echo stream. The only
tricky bit is determining whether or not to include the last character
so echoed: it could be a delimiter, in which case we don't want it.

@<Define token reading...@>=
(defun token-delimiter-p (char)
  (declare (type character char))
  (or (whitespacep char)
      (multiple-value-bind (function non-terminating-p) ;
          (get-macro-character char)
        (and function (not non-terminating-p)))))

(defun read-token (stream)
  (let* ((chars (with-output-to-string (string-stream)
                  (let ((echo-stream (make-echo-stream stream string-stream))
                        (*read-suppress* t))
                    (read echo-stream))))
         (n (length chars)))
    (subseq chars 0 (if (token-delimiter-p (char chars (1- n))) (1- n) n))))

@ Now that we can read tokens, we turn to parsing them as symbols.
Symbol-designating tokens consist of three parts: an optional package
prefix, up to two package markers, and a symbol name (see~\S2.3.5 of the
Common Lisp standard for the exact rules). We assume that \.{:} is the
package marker, \.{\\} is the (unique) single escape character, and
\.{\char'174} is the (unique) multiple escape character. We completely
ignore the whole notion of potential numbers.

@<Define symbol parsing...@>=
(defun parse-symbol (token &aux (token (string token)))
  (do* ((i 0 (1+ i))
        (n (length token))
        (buf (make-array n :element-type 'character :fill-pointer 0))
        (single-escape) ; escape the next character
        (multiple-escape) ; escape the following characters
        (escaped '()) ; list of escaped indices
        (markers '()) ; list of package marker indices (at most 2)
        (offset 0) ; difference between normalized and raw indices
        (all-upper t) ; unescaped character case
        (all-lower t))
       ((= i n) (setq escaped (nreverse escaped)
                      markers (nreverse markers))
                @<Case-fold the unescaped characters in |buf|@>
                @<Construct and return a |raw-symbol| instance@>)
    (let ((char (char token i)))
      (cond (single-escape
             (push (vector-push char buf) escaped)
             (setq single-escape nil))
            ((char= char #\\)
             (setq single-escape t))
            ((char= char #\|)
             (setq multiple-escape (not multiple-escape)))
            (multiple-escape
             (push (vector-push char buf) escaped))
            ((char= char #\:)
             (let ((marker (vector-push char buf)))
               (when (or (> (length markers) 1) ;
                         (and (first markers) ;
                              (/= marker (1+ (first markers)))))
                 (error "Too many colons in ~S." token))
               (push marker markers)
               (setq offset (- marker i))))
            (t (vector-push char buf)
               (when (both-case-p char)
                 (if (upper-case-p char)
                     (setq all-lower nil)
                     (setq all-upper nil))))))))

@ @<Construct and return...@>=
(let ((print-case (if all-lower :downcase *print-case*)))
  (if markers
      (destructuring-bind (i &optional (j i)) markers
        (make-raw-symbol :package (if (plusp i) (subseq buf 0 i) "KEYWORD")
                         :internal (string= (subseq buf i (1+ j)) "::")
                         :name (subseq buf (1+ j))
                         :prefix (subseq token 0 (- i offset))
                         :markers (subseq token (- i offset) (1+ (- j offset)))
                         :suffix (subseq token (1+ (- j offset)))
                         :print-case print-case))
      (make-raw-symbol :name buf :suffix token :print-case print-case)))

@t@l
(deftest (parse-symbol bare)
  (equalp (parse-symbol "foo")
          (make-raw-symbol :name "FOO" :suffix "foo"))
  t)

(deftest (parse-symbol keyword)
  (equalp (parse-symbol ":foo")
          (make-raw-symbol :package "KEYWORD" :name "FOO"
                           :prefix "" :markers ":" :suffix "foo"))
  t)

(deftest (parse-symbol external)
  (equalp (parse-symbol "foo:bar")
          (make-raw-symbol :package "FOO" :name "BAR"
                           :prefix "foo" :markers ":" :suffix "bar"))
  t)

(deftest (parse-symbol internal)
  (equalp (parse-symbol "FOO::BAR")
          (make-raw-symbol :package "FOO" :internal t :name "BAR"
                           :prefix "FOO" :markers "::" :suffix "BAR"
                           :print-case :upcase))
  t)

(deftest (parse-symbol escaped)
  (equalp (parse-symbol "f\\oo|::barbaz|::G\\:rack")
          (make-raw-symbol :package "FoO::barbaz"
                           :internal t
                           :name "G:RACK"
                           :prefix "f\\oo|::barbaz|"
                           :markers "::"
                           :suffix "G\\:rack"
                           :print-case :upcase))
  t)

(deftest (parse-symbol error)
  (handler-case (parse-symbol ":foo:")
    (error () t))
  t)

@ Next we implement the case folding rules specified by \S23.1.2 of the
{\sc ansi} Common Lisp standard (`Effect of Readtable Case on the Lisp
Reader').

@<Case-fold...@>=
(labels ((escaped (i) (member i escaped :test #'=))
         (fold (transform)
           (loop for ch across buf and i upfrom 0
                 unless (escaped i)
                   do (setf (char buf i) (funcall transform ch))))
         (lower () (fold #'char-downcase))
         (raise () (fold #'char-upcase)))
  (ecase (readtable-case *readtable*)
    (:upcase (raise))
    (:downcase (lower))
    (:preserve)
    (:invert (cond (all-lower (raise))
                   (all-upper (lower))))))

@t@l
(defun parse-symbol-with-case (token case)
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) case)
    (parse-symbol token)))

(deftest (parse-symbol :downcase)
  (equalp (parse-symbol-with-case "FOO:\\XBAR" :downcase)
          (make-raw-symbol :package "foo" :name "Xbar"
                           :prefix "FOO" :markers ":" :suffix "\\XBAR"
                           :print-case :upcase))
  t)

(deftest (parse-symbol :preserve)
  (equalp (parse-symbol-with-case "FOO:\\xbar" :preserve)
          (make-raw-symbol :package "FOO" :name "xbar"
                           :prefix "FOO" :markers ":" :suffix "\\xbar"
                           :print-case :upcase))
  t)

(deftest (parse-symbol :invert upper)
  (equalp (parse-symbol-with-case "FOO:\\XBAR" :invert)
          (make-raw-symbol :package "foo" :name "Xbar"
                           :prefix "FOO" :markers ":" :suffix "\\XBAR"
                           :print-case :upcase))
  t)

(deftest (parse-symbol :invert lower)
  (equalp (parse-symbol-with-case "foo:\\xbar" :invert)
          (make-raw-symbol :package "FOO" :name "xBAR"
                           :prefix "foo" :markers ":" :suffix "\\xbar"))
  t)

(deftest (parse-symbol :invert mixed)
  (equalp (parse-symbol-with-case "Foo:\\xBar" :invert)
          (make-raw-symbol :package "Foo" :name "xBar"
                           :prefix "Foo" :markers ":" :suffix "\\xBar"
                           :print-case :upcase))
  t)

@ We look up raw symbols using |find-raw-symbol|, which signals an error if
either the designated symbol does not exist or the specified access doesn't
match the symbol's accessibility in the designated package.

@l
(defun find-package-or-lose (package-name)
  (if package-name
      (or (find-package package-name)
          (error 'no-such-package-error :package package-name))
      *package*))

(defun raw-symbol-accessible-p (raw-symbol accessibility)
  (or (null (raw-symbol-package raw-symbol))
      (raw-symbol-internal raw-symbol)
      (eq accessibility :external)))

(defun find-raw-symbol (raw-symbol)
  (let ((package (find-package-or-lose (raw-symbol-package raw-symbol)))
        (name (raw-symbol-name raw-symbol)))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (unless symbol
        (error 'no-such-symbol-error :package package :name name))
      (unless (raw-symbol-accessible-p raw-symbol status)
        (cerror "Use the symbol anyway."
                'symbol-accessibility-error :package package :name name))
      (values symbol status))))

@t Don't name a package \.{"XXXXXX"}, ok?

@l
(deftest (find-raw-symbol ok)
  (find-raw-symbol (parse-symbol "sludge::find-raw-symbol"))
  find-raw-symbol
  :internal)

(deftest (find-raw-symbol no-package-error)
  (handler-case (find-raw-symbol (parse-symbol "xxxxxx:foo"))
    (package-error () t))
  t)

(deftest (find-raw-symbol accessibility-error)
  (handler-case (find-raw-symbol (parse-symbol "sludge:find-raw-symbol"))
    (package-error () t))
  t)

@ @<Condition classes@>=
(define-condition no-such-package-error (package-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No such package: ~A." ;
                     (package-error-package condition)))))

(define-condition no-such-symbol-error (package-error)
  ((symbol-name :accessor package-error-symbol-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "There is no symbol named ~S in package ~A."
                     (package-error-symbol-name condition)
                     (package-name (package-error-package condition))))))

(define-condition symbol-accessibility-error (package-error)
  ((symbol-name :accessor package-error-symbol-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "The symbol ~S is not external in package ~A."
                     (package-error-symbol-name condition)
                     (package-name (package-error-package condition))))))

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

@ We now have all of the pieces in place to define the core of our main
loop. If \eof\ is encountered during the read, we exit the loop. If an
error occurs during request handling, we send an error response and
continue processing with the next message. If the client signals their
desire to disconnect, we acknowledge the request and exit the loop. We also
establish a |continue| restart which simply ignores the current request and
picks up with the next one.

@<Read and handle a request@>=
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
          (send-error-message code tag condition))))))

@1*Request handlers. These tend to follow a similar pattern, so we'll use a
defining macro that abstracts it a bit. The request arguments are bound
using |destructuring-bind| to the parameters specified by |lambda-list|,
and the body should return a designator for a list of response arguments,
from which a response message will be constructed and sent. We'll leave it
up to the main loop to handle any errors by constructing and sending error
responses.

@l
(defmacro define-request-handler (request-code lambda-list &body body)
  "Define a method on HANDLE-REQUEST, EQL-specialized on REQUEST-CODE."
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
  (let ((function (typecase function
                    (raw-symbol (find-raw-symbol function))
                    (t function))))
    (list (function-lambda-list function))))

@ Other pieces of documentation that might be requested are docstrings
and object descriptions.

@l
(define-request-handler :documentation (x doc-type)
  (list (documentation x doc-type)))

(define-request-handler :describe (object)
  (list (with-output-to-string (*standard-output*) (describe object))))

@ This next command allows the client to set the current package. In order
to correctly obtain information about objects named by non-package-prefixed
symbols, the current package must match the implicit package of the given
symbol.

@l
(define-request-handler :in-package (name)
  (let ((package (find-package-or-lose name)))
    (setq *package* package)
    (package-name package)))

@ Now we'll define the protocol message that implements the second main
reason for this system's existence: Lisp symbol completion.

@l
(defun string-prefix-p (prefix string)
  (let ((l (length (string prefix)))
        (m (length (string string))))
    (and (>= m l)
         (let ((n (min l m)))
           (string-equal prefix string :end1 n :end2 n)))))

(defun match-symbols (raw-symbol)
  (let* ((package-name (raw-symbol-package raw-symbol))
         (package-prefix (raw-symbol-prefix raw-symbol))
         (markers (raw-symbol-markers raw-symbol))
         (name (raw-symbol-name raw-symbol))
         (*package* (find-package-or-lose package-name))
         (*print-case* (raw-symbol-print-case raw-symbol)))
    (labels ((format-symbol (symbol)
               (format nil "~@[~A~]~@[~A~]~A" package-prefix markers symbol)))
      (with-package-iterator (next *package* :internal :external :inherited)
        (let ((matches '()))
          (loop
            (multiple-value-bind (more symbol accessibility) (next)
              (unless more
                (return (sort (mapcar #'format-symbol ;
                                      (remove-duplicates matches)) ;
                              #'string<)))
              (when (and (raw-symbol-accessible-p raw-symbol accessibility)
                         (string-prefix-p name symbol))
                (push symbol matches)))))))))

(define-request-handler :symbol-completions (raw-symbol)
  (match-symbols raw-symbol))

@t@l
(deftest string-prefix-p
  (values (string-prefix-p "" "foo")
          (string-prefix-p "f" "foo")
          (string-prefix-p "foo" "foo")
          (string-prefix-p "foo-bar" "foo")
          (string-prefix-p "abc" "foo"))
  t
  t
  t
  nil
  nil)

@ Clients are free to just disconnect when they're done, but they might
prefer to be polite about it and ask that the connection be terminated.

@l
(define-request-handler :bye (&rest args)
  (signal 'client-disconnect :args args))

@ @<Condition classes@>=
(define-condition client-disconnect ()
  ((args :reader client-disconnect-args :initarg :args)))

@*Index.
@t*Index.
