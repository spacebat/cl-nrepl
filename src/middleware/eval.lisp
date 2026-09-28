(in-package :nrepl)

(defun run-evaluation (message thunk)
  "Run the evaluation `thunk` for `message` in the current session.

  Registered sessions queue it on their worker thread, so the connection can
  keep handling messages (like `interrupt` and `stdin`) while it runs.
  Ephemeral sessions just run it here.

  "
  (let ((session *session*))
    (if (session-ephemeral session)
      (funcall thunk)
      (session-submit session
                      (lambda ()
                        (let ((*session* session))
                          (call-reporting-errors message thunk)))))))

(define-middleware wrap-eval "eval" message
  "Evaluates code.  Requires \"code\"; optionally \"ns\", the package to
  evaluate in (defaults to the session's).  Returns a \"value\" and \"ns\" for
  each form, \"out\"/\"err\" for output, and \"ex\"/\"root-ex\" with an
  \"eval-error\" status for errors."
  (let ((code (fset:lookup message "code")))
    (if code
      (run-evaluation message
                      (lambda ()
                        (evaluate-forms message code
                                        :ns (or (fset:lookup message "ns")
                                                (fset:lookup message "in-package")))))
      (respond message (make-map "status" '("done" "no-code" "error"))))))
