(in-package :nrepl)

(defun wrap-session (handler)
  "Handle wrapping incoming messages in sessions.

  If a message contains a session key, look up that session in the list of
  registered sessions and bind it into *session*.  If there's no such session,
  respond with an `unknown-session` error.

  If a message comes in without a session id, create a new ephemeral session
  for it and patch the session id into the message before continuing on down
  the middleware stack.  Also binds the session into *session*.

  Ephemeral sessions are NOT registered into the main map of sessions.  To get
  a session that'll stick around the client needs to do a `clone` op.

  "
  (lambda (message)
    (let* ((session-id (fset:lookup message "session"))
           (session (if session-id
                      (get-session session-id)
                      (make-session :ephemeral t))))
      (if session
        (let ((*session* session))
          (funcall handler
                   (fset:with message "session" (session-id session))))
        (respond message
                 (make-map "status" '("done" "unknown-session" "error")))))))

(define-middleware wrap-session-ls "ls-sessions" message
  "Lists the ids of all registered sessions, as \"sessions\"."
  (respond message
           (make-map "status" '("done")
                     "sessions" (get-sessions))))

(define-middleware wrap-session-close "close" message
  "Closes the session, interrupting anything running in it."
  (remove-session! (session-id *session*))
  (close-session *session*)
  (respond message (make-map "status" '("done" "session-closed"))))

(define-middleware wrap-session-clone "clone" message
  "Creates a new session, copying the current one's state (or starting fresh
  if there's no session).  Returns its id as \"new-session\"."
  (let ((new-session (make-session :parent (unless (session-ephemeral *session*)
                                             *session*))))
    (register-session! new-session)
    (respond message (make-map "status" '("done")
                               "new-session" (session-id new-session)))))

(define-middleware wrap-interrupt "interrupt" message
  "Interrupts the evaluation running in the session.  Optionally takes
  \"interrupt-id\", the id of the message to interrupt.  Status is
  \"interrupted\", \"session-idle\", \"interrupt-id-mismatch\" or
  \"session-ephemeral\"."
  (respond message
           (make-map "status"
                     (if (session-ephemeral *session*)
                       '("done" "session-ephemeral" "error")
                       (ecase (interrupt-session *session*
                                                 (fset:lookup message "interrupt-id"))
                         (:interrupted '("done" "interrupted"))
                         (:idle '("done" "session-idle"))
                         (:mismatch '("done" "interrupt-id-mismatch" "error")))))))

(define-middleware wrap-stdin "stdin" message
  "Adds the \"stdin\" string to the session's `*standard-input*`.  An empty
  string signals end of file."
  (if (session-ephemeral *session*)
    (respond message (make-map "status" '("done" "session-ephemeral" "error")))
    (progn
      (add-input (session-input *session*) (or (fset:lookup message "stdin") ""))
      (respond message (make-map "status" '("done"))))))
