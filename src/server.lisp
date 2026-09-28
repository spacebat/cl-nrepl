(in-package :nrepl)

;;;; Config
(defvar *verbose-debug-output* nil)
(defvar *unthreaded* nil)


;;;; Plumbing
(defun handle-base (message)
  "Base handler function.

  Just returns a fallback response if no earlier middleware handled the message.

  "
  (respond message (make-map "status" '("done" "unknown-op" "error")
                             "op" (fset:lookup message "op"))))

(defun middleware ()
  "Return the stack of middleware.

   In the future we should make this less horrifyingly inefficient, but for
   NREPL development its_fine.

   "
  (list
    'wrap-session
    'wrap-session-ls
    'wrap-session-clone
    'wrap-session-close
    'wrap-interrupt
    'wrap-stdin
    'wrap-describe
    'wrap-load-file
    'wrap-macroexpand
    'wrap-eval
    'wrap-documentation
    'wrap-arglist
    'wrap-lookup
    'wrap-completions
    ))

(defun build-handler (base middleware)
  "Collapse the stack of `middleware` into a single handler function."
  (if middleware
    (funcall (car middleware)
             (build-handler base (cdr middleware)))
    base))

(defun handle (message)
  "Handle the given NREPL message."
  (when *verbose-debug-output*
    (log-message "~%; Handling message: vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv~%")
    (log-message "~S~%" message)
    (log-message "; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~%"))
  (funcall (build-handler #'handle-base (middleware)) message))

(defun handle-message (message)
  "Handle `message`, responding with an error instead of signaling one."
  (call-reporting-errors message (lambda () (handle message))))

(defun handler (socket-stream lock)
  "Read a series of messages from `socket-stream`, handling each."
  (log-message "Client connected...~%")
  (loop
    (let ((message (handler-case (read-object socket-stream)
                     (end-of-file () (return))
                     (error (e)
                       ;; The stream is in an unknown state after a failed
                       ;; read, so there's no sensible way to continue.
                       (log-message "~%; Error reading message: ~A~%" e)
                       (return)))))
      (handle-message
        (fset:with message "transport"
                   (curry #'write-object socket-stream lock)))))
  (log-message "Client disconnected...~%"))


;;;; Server
(defvar *server-thread* nil)

(defmacro run-in-thread (thread-name &rest body)
  "Run `body` in a thread called `name` (usually).  Return the thread.

  If `nrepl::*unthreaded*` is true, the body will be executed immediately in the
  current thread and `nil` will be returned instead.  Useful for debugging.

  "
  `(let ((thunk (lambda () ,@body)))
    (if *unthreaded*
      (progn (funcall thunk) nil)
      (bt:make-thread thunk :name ,thread-name))))


(defun accept-connections (server-socket)
  "Accept connections to the server and spawn threads to handle each."
  (log-message "Accepting connections...~%")
  (loop
    (let* ((client-socket (usocket:socket-accept
                            server-socket
                            :element-type '(unsigned-byte 8)))
           (socket-stream (flex:make-flexi-stream
                            (usocket:socket-stream client-socket)
                            :external-format :utf-8))
           (write-lock (bt:make-lock "NREPL client writing lock")))
      (run-in-thread "NREPL Connection Handler"
        (unwind-protect
            (handler socket-stream write-lock)
          (usocket:socket-close client-socket))))))

(defun start-server (&key (address "127.0.0.1") (port 8675))
  "Fire up a server thread that will listen for connections.

  Returns the server thread and the port actually listened on (useful when
  `port` is 0, which asks the OS for a free port).

  "
  (log-message "Starting server...~%")
  (let ((socket (usocket:socket-listen
                  address port
                  :reuse-address t
                  ;; have to specify element-type here too because usocket+CCL
                  ;; fucks it up if you only specify it in socket-accept
                  :element-type '(unsigned-byte 8))))
    (let ((port (usocket:get-local-port socket)))
      (values (setf *server-thread*
                    (run-in-thread (format nil "NREPL Server (~a/~a)" address port)
                      (unwind-protect
                          (accept-connections socket)
                        (log-message "Closing server socket...~%")
                        (usocket:socket-close socket))))
              port))))

(defun stop-server ()
  "Kill the server thread, if it exists, and wait for it to finish."
  (let ((s (shiftf *server-thread* nil)))
    (when s
      (log-message "Stopping server...~%")
      (when (bt:thread-alive-p s)
        (bt:destroy-thread s)
        (ignore-errors (bt:join-thread s))))))

