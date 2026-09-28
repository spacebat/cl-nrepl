(in-package :nrepl)

(defun wrap-session (handler)
  "Handle wrapping incoming messages in sessions.

  If a message contains a session key, look up that session in the list of
  registered sessions and bind it into *session*.  If there's no such session,
  respond with an `unknown-session` error.

  If a message comes in without a session id, create a new session for it and
  patch the session id into the message before continuing on down the
  middleware stack.  Also binds the session into *session*.

  Note that this implicit creation will NOT register the session into the main
  map of sessions.  To register the session so it'll stick around the client
  needs to do a `clone` op.

  "
  (lambda (message)
    (let* ((session-id (fset:lookup message "session"))
           (session (if session-id
                      (get-session session-id)
                      (make-session))))
      (if session
        (let ((*session* session))
          (funcall handler
                   (fset:with message "session" (session-id session))))
        (respond message (make-map "status" '("error" "unknown-session")))))))

(define-middleware wrap-session-ls "ls-sessions" message
  (respond message
           (make-map "status" '("done")
                     "sessions" (get-sessions))))

(define-middleware wrap-session-close "close" message
  (remove-session! (fset:lookup message "session"))
  (respond message (make-map "status" '("session-closed"))))

(define-middleware wrap-session-clone "clone" message
  (let ((new-session (make-session *session*)))
    (register-session! new-session)
    (respond message (make-map "status" '("done")
                               "new-session" (session-id new-session)))))
