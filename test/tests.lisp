(in-package :nrepl.test)

(def-suite :nrepl)
(in-suite :nrepl)

(defun run-tests ()
  "Start a server on a free port, run the suite against it, and return whether
  every test passed."
  ;; Set the log globally rather than binding it: server threads don't see
  ;; our dynamic bindings.
  (let ((old-log nrepl::*log*))
    (setf nrepl::*log* (make-broadcast-stream))
    (multiple-value-bind (thread port) (nrepl:start-server :port 0)
      (declare (ignore thread))
      (unwind-protect
          (let ((*port* port))
            (run! :nrepl))
        (ignore-errors (nrepl:stop-server))
        (setf nrepl::*log* old-log)))))

(defun eval-in (connection code &rest keyvals)
  (apply #'send connection "op" "eval" "code" code keyvals))

(defun clone (connection &optional session)
  (first (field "new-session"
                (send connection "op" "clone" "session" session))))


;;;; Eval
(test eval-simple
  (with-connection (c)
    (let ((r (eval-in c "(+ 1 2)")))
      (is (equal "3" (value r)))
      (is (member "done" (statuses r) :test #'equal)))))

(test eval-multiple-forms
  (with-connection (c)
    (let ((r (eval-in c "(+ 1 2) (* 2 3)")))
      (is (equal '("3" "6") (field "value" r))))))

(test eval-trailing-whitespace-and-comments
  (with-connection (c)
    (let ((r (eval-in c (format nil "(+ 1 2)~%  ; a comment~%~C" #\Tab))))
      (is (equal '("3") (field "value" r)))
      (is (not (member "error" (statuses r) :test #'equal))))))

(test eval-in-package-affects-later-forms
  (with-connection (c)
    (let ((r (eval-in c "(in-package :nrepl) (package-name (symbol-package 'foo))")))
      (is (equal "\"NREPL\"" (value r))))))

(test eval-stdin-is-empty
  (with-connection (c)
    (is (equal ":EOF" (value (eval-in c "(read-line *standard-input* nil :eof)"))))))


;;;; Output
(test stdout-arrives-before-done
  (with-connection (c)
    (let ((r (eval-in c "(progn (princ \"hello\") (terpri) (princ \"no newline\") 42)")))
      (is (equal (format nil "hello~%no newline") (joined "stdout" r)))
      (is (< (position-of (lambda (m) (nth-value 1 (gethash "stdout" m))) r)
             (position-of (lambda (m) (nth-value 1 (gethash "value" m))) r)))
      (is (equal "42" (value r))))))

(test stderr-captured
  (with-connection (c)
    (let ((r (eval-in c "(progn (princ \"oops\" *error-output*) nil)")))
      (is (equal "oops" (joined "stderr" r))))))

(test lots-of-output
  (with-connection (c)
    (let ((r (eval-in c "(dotimes (i 1000) (print i))")))
      (is (= 1000 (count #\Newline (joined "stdout" r))))
      (is (equal "NIL" (value r))))))


;;;; Errors
(test eval-error-keeps-connection
  (with-connection (c)
    (let ((r (eval-in c "(error \"boom\")")))
      (is (member "error" (statuses r) :test #'equal)))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test malformed-eval-input
  (with-connection (c)
    (is (member "error" (statuses (eval-in c "(+ 1")) :test #'equal))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test malformed-macroexpand-keeps-connection
  (with-connection (c)
    (let ((r (send c "op" "macroexpand" "form" "(when")))
      (is (member "error" (statuses r) :test #'equal)))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test unknown-op
  (with-connection (c)
    (is (member "unknown-op" (statuses (send c "op" "no-such-op"))
                :test #'equal))))


;;;; Sessions
(test clone-returns-new-registered-session
  (with-connection (c)
    (let* ((a (clone c))
           (b (clone c a))
           (sessions (first (field "sessions" (send c "op" "ls-sessions")))))
      (is (stringp a))
      (is (stringp b))
      (is (not (equal a b)))
      (is (member a sessions :test #'equal))
      (is (member b sessions :test #'equal)))))

(test session-remembers-package
  (with-connection (c)
    (let ((s (clone c)))
      (eval-in c "(in-package :nrepl)" "session" s)
      (is (equal "\"NREPL\"" (value (eval-in c "(package-name *package*)"
                                             "session" s)))))))

(test sessions-are-isolated
  (with-connection (c)
    (let ((a (clone c))
          (b (clone c)))
      (eval-in c "(in-package :nrepl)" "session" a)
      (is (equal "\"COMMON-LISP-USER\""
                 (value (eval-in c "(package-name *package*)" "session" b)))))))

(test clone-copies-state
  (with-connection (c)
    (let ((a (clone c)))
      (eval-in c "(in-package :nrepl)" "session" a)
      (let ((b (clone c a)))
        (is (equal "\"NREPL\""
                   (value (eval-in c "(package-name *package*)"
                                   "session" b))))))))

(test session-history-variables
  (with-connection (c)
    (let ((s (clone c)))
      (eval-in c "(+ 40 2)" "session" s)
      (is (equal "42" (value (eval-in c "*" "session" s)))))))

(test documentation-uses-session-package
  (with-connection (c)
    (let ((s (clone c)))
      (eval-in c "(in-package :nrepl)" "session" s)
      (let ((r (send c "op" "arglist" "symbol" "respond" "session" s)))
        (is (search "RESPOND" (first (field "function-arglist" r))))))))

(test unknown-session
  (with-connection (c)
    (is (member "unknown-session"
                (statuses (eval-in c "1" "session" "no-such-session"))
                :test #'equal))))

(test close-session
  (with-connection (c)
    (let ((s (clone c)))
      (is (member "session-closed" (statuses (send c "op" "close" "session" s))
                  :test #'equal))
      (is (not (member s (first (field "sessions" (send c "op" "ls-sessions")))
                       :test #'equal))))))


;;;; Load file
(test load-file-contents
  (with-connection (c)
    (let ((r (send c "op" "load-file"
                   "file" (format nil "(in-package :cl-user)~%(defparameter *nrepl-load-test* 42)~%")
                   "file-name" "test.lisp")))
      (is (not (member "error" (statuses r) :test #'equal)))
      (is (equal "42" (value (eval-in c "cl-user::*nrepl-load-test*")))))))
