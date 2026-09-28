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

(defun first-value (responses)
  "The first line of the last value: just the primary value, when a form
  returns several."
  (let ((v (value responses)))
    (subseq v 0 (position #\Newline v))))

(defun has-status (status responses)
  (member status (statuses responses) :test #'equal))

(defun responses-until-output (connection id)
  "Wait for message `id` to produce some output, so it's known to be running."
  (loop :for response = (next-response connection id)
        :collect response
        :until (nth-value 1 (gethash "out" response))))

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

(test eval-returns-ns
  (with-connection (c)
    (is (equal '("COMMON-LISP-USER") (field "ns" (eval-in c "1"))))
    (let ((r (eval-in c "(package-name *package*)" "ns" "nrepl")))
      (is (equal "\"NREPL\"" (value r)))
      (is (equal '("NREPL") (field "ns" r))))))

(test eval-multiple-values
  (with-connection (c)
    (is (equal (format nil "1~%2") (value (eval-in c "(values 1 2)"))))
    (is (equal "; No values" (value (eval-in c "(values)"))))))

(test eval-stdin-is-empty
  (with-connection (c)
    (is (equal ":EOF" (first-value (eval-in c "(read-line *standard-input* nil :eof)"))))))


;;;; Output
(test stdout-arrives-before-done
  (with-connection (c)
    (let ((r (eval-in c "(progn (princ \"hello\") (terpri) (princ \"no newline\") 42)")))
      (is (equal (format nil "hello~%no newline") (joined "out" r)))
      (is (< (position-of (lambda (m) (nth-value 1 (gethash "out" m))) r)
             (position-of (lambda (m) (nth-value 1 (gethash "value" m))) r)))
      (is (equal "42" (value r))))))

(test stderr-captured
  (with-connection (c)
    (let ((r (eval-in c "(progn (princ \"oops\" *error-output*) nil)")))
      (is (equal "oops" (joined "err" r))))))

(test lots-of-output
  (with-connection (c)
    (let ((r (eval-in c "(dotimes (i 1000) (print i))")))
      (is (= 1000 (count #\Newline (joined "out" r))))
      (is (equal "NIL" (value r))))))


;;;; Errors
(test eval-error
  (with-connection (c)
    (let ((r (eval-in c "(error \"boom\")")))
      (is (has-status "eval-error" r))
      (is (has-status "done" r))
      (is (equal '("COMMON-LISP:SIMPLE-ERROR") (field "ex" r)))
      (is (equal '("COMMON-LISP:SIMPLE-ERROR") (field "root-ex" r)))
      (is (search "boom" (joined "err" r))))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test eval-continues-after-error
  ;; Like nREPL: an error evaluating a form doesn't stop the later forms.
  (with-connection (c)
    (let ((r (eval-in c "1 (error \"boom\") 3")))
      (is (equal '("1" "3") (field "value" r)))
      (is (has-status "eval-error" r)))))

(test malformed-eval-input
  ;; Like nREPL: a read error stops evaluation.
  (with-connection (c)
    (let ((r (eval-in c "1 (+ 1")))
      (is (equal '("1") (field "value" r)))
      (is (has-status "eval-error" r))
      (is (has-status "done" r)))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test eval-without-code
  (with-connection (c)
    (let ((r (send c "op" "eval")))
      (is (has-status "no-code" r))
      (is (has-status "error" r)))))

(test malformed-macroexpand-keeps-connection
  (with-connection (c)
    (let ((r (send c "op" "macroexpand" "form" "(when")))
      (is (has-status "error" r))
      (is (has-status "done" r)))
    (is (equal "2" (value (eval-in c "(+ 1 1)"))))))

(test unknown-op
  (with-connection (c)
    (let ((r (send c "op" "no-such-op")))
      (is (has-status "unknown-op" r))
      (is (has-status "error" r))
      (is (equal '("no-such-op") (field "op" r))))))


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
    (let ((r (eval-in c "1" "session" "no-such-session")))
      (is (has-status "unknown-session" r))
      (is (has-status "error" r)))))

(test evaluations-run-in-order
  (with-connection (c)
    (let* ((s (clone c))
           (a (send-message c "op" "eval" "session" s
                            "code" "(progn (sleep 0.3) (defparameter *order* (list 1)))"))
           (b (send-message c "op" "eval" "session" s
                            "code" "(push 2 *order*)")))
      (responses c a)
      (is (equal "(2 1)" (value (responses c b)))))))

(test close-session
  (with-connection (c)
    (let ((s (clone c)))
      (is (has-status "session-closed" (send c "op" "close" "session" s)))
      (is (not (member s (first (field "sessions" (send c "op" "ls-sessions")))
                       :test #'equal))))))


;;;; Load file
(test load-file-contents
  (with-connection (c)
    (let ((r (send c "op" "load-file"
                   "file" (format nil "(in-package :nrepl)~%(defparameter cl-user::*nrepl-load-test* 42)~%(+ 1 2)~%")
                   "file-name" "test.lisp")))
      (is (not (has-status "eval-error" r)))
      (is (equal "3" (value r)))
      (is (equal "42" (value (eval-in c "cl-user::*nrepl-load-test*"))))
      ;; The file's in-package doesn't leak out.
      (is (equal "\"COMMON-LISP-USER\"" (value (eval-in c "(package-name *package*)")))))))

(test load-file-error
  (with-connection (c)
    (let ((r (send c "op" "load-file" "file" "(error \"bad file\")")))
      (is (has-status "eval-error" r))
      (is (has-status "done" r)))))


;;;; Interrupt
(test interrupt-running-eval
  (with-connection (c)
    (let* ((s (clone c))
           (e (send-message c "op" "eval" "session" s
                            "code" "(progn (print :started) (finish-output) (loop (sleep 0.01)))")))
      ;; Wait until it's definitely running.
      (responses-until-output c e)
      (let ((i (send c "op" "interrupt" "session" s "interrupt-id" e)))
        (is (has-status "interrupted" i))
        (is (has-status "done" i)))
      (let ((r (responses c e)))
        (is (has-status "interrupted" r)))
      ;; The session still works afterwards.
      (is (equal "2" (value (eval-in c "(+ 1 1)" "session" s)))))))

(test interrupt-without-id
  (with-connection (c)
    (let* ((s (clone c))
           (e (send-message c "op" "eval" "session" s
                            "code" "(progn (print :started) (finish-output) (loop (sleep 0.01)))")))
      (responses-until-output c e)
      (is (has-status "interrupted" (send c "op" "interrupt" "session" s)))
      (is (has-status "interrupted" (responses c e))))))

(test interrupt-idle-session
  (with-connection (c)
    (let ((s (clone c)))
      (is (has-status "session-idle" (send c "op" "interrupt" "session" s))))))

(test interrupt-id-mismatch
  (with-connection (c)
    (let* ((s (clone c))
           ;; Block on stdin rather than sleeping, so the eval is certainly
           ;; still running when the interrupt arrives, however slow things are.
           (e (send-message c "op" "eval" "session" s
                            "code" "(progn (read-line) :finished)")))
      (responses-until c e "need-input")
      (let ((i (send c "op" "interrupt" "session" s "interrupt-id" "not-it")))
        (is (has-status "interrupt-id-mismatch" i)))
      (send c "op" "stdin" "session" s "stdin" (format nil "go~%"))
      (is (equal ":FINISHED" (value (responses c e)))))))

(test interrupt-ephemeral-session
  (with-connection (c)
    (is (has-status "session-ephemeral" (send c "op" "interrupt")))))

(test close-interrupts-running-eval
  (with-connection (c)
    (let* ((s (clone c))
           (e (send-message c "op" "eval" "session" s
                            "code" "(progn (print :started) (finish-output) (loop (sleep 0.01)))")))
      (responses-until-output c e)
      (send c "op" "close" "session" s)
      (is (has-status "interrupted" (responses c e))))))


;;;; Stdin
(test stdin-need-input
  (with-connection (c)
    (let* ((s (clone c))
           (e (send-message c "op" "eval" "session" s "code" "(read-line)")))
      (is (has-status "need-input" (responses-until c e "need-input")))
      (is (has-status "done" (send c "op" "stdin" "session" s
                                   "stdin" (format nil "hello there~%"))))
      ;; read-line's second value is whether the newline was missing.
      (is (equal (format nil "\"hello there\"~%NIL") (value (responses c e)))))))

(test stdin-sent-ahead
  (with-connection (c)
    (let ((s (clone c)))
      (send c "op" "stdin" "session" s "stdin" (format nil "(1 2 3)~%"))
      (is (equal "(1 2 3)" (value (eval-in c "(read)" "session" s)))))))

(test stdin-eof
  (with-connection (c)
    (let* ((s (clone c))
           (e (send-message c "op" "eval" "session" s
                            "code" "(read-line *standard-input* nil :eof)")))
      (responses-until c e "need-input")
      (send c "op" "stdin" "session" s "stdin" "")
      (is (equal ":EOF" (first-value (responses c e)))))))


;;;; Describe
(test describe-ops
  (with-connection (c)
    (let* ((r (first (send c "op" "describe")))
           (ops (gethash "ops" r)))
      (dolist (op '("eval" "load-file" "interrupt" "stdin" "clone" "close"
                    "ls-sessions" "describe" "completions" "lookup"))
        (is-true (nth-value 1 (gethash op ops)) "describe is missing ~A" op))
      (is (equal "0.0.1" (gethash "version-string"
                                  (gethash "cl-nrepl" (gethash "versions" r)))))
      (is (stringp (gethash "version-string"
                            (gethash "lisp" (gethash "versions" r)))))
      (is (equal "COMMON-LISP-USER"
                 (gethash "current-ns" (gethash "aux" r)))))))

(test describe-verbose
  (with-connection (c)
    (let ((ops (gethash "ops" (first (send c "op" "describe" "verbose?" "true")))))
      (is (search "Evaluates" (gethash "doc" (gethash "eval" ops)))))))


;;;; Lookup
(test lookup-function
  (with-connection (c)
    (let ((info (first (field "info" (send c "op" "lookup" "sym" "mapcar")))))
      (is (equal "mapcar" (gethash "name" info)))
      (is (equal "COMMON-LISP" (gethash "ns" info)))
      (is (equal "function" (gethash "type" info)))
      (is (search "function" (gethash "arglists-str" info)))
      (is (stringp (gethash "doc" info))))))

(test lookup-in-package
  (with-connection (c)
    (let ((info (first (field "info" (send c "op" "lookup" "sym" "respond"
                                           "ns" "nrepl")))))
      (is (equal "respond" (gethash "name" info)))
      (is (equal "NREPL" (gethash "ns" info))))))

(test lookup-unknown
  (with-connection (c)
    (let ((r (send c "op" "lookup" "sym" "no-such-symbol-anywhere")))
      (is (has-status "done" r))
      (is (zerop (hash-table-count (first (field "info" r))))))))


;;;; Completions
(defun candidates (responses)
  (mapcar (lambda (c) (gethash "candidate" c))
          (first (field "completions" responses))))

(test complete-symbols
  (with-connection (c)
    (let* ((r (send c "op" "completions" "prefix" "mapc"))
           (mapcar (find "mapcar" (first (field "completions" r))
                         :key (lambda (c) (gethash "candidate" c))
                         :test #'equal)))
      (is (member "mapcan" (candidates r) :test #'equal))
      (is (equal "function" (gethash "type" mapcar)))
      (is (equal "COMMON-LISP" (gethash "ns" mapcar))))))

(test complete-kinds
  (with-connection (c)
    (flet ((type-of-candidate (prefix candidate)
             (gethash "type" (find candidate
                                   (first (field "completions"
                                                 (send c "op" "completions"
                                                       "prefix" prefix)))
                                   :key (lambda (c) (gethash "candidate" c))
                                   :test #'equal))))
      (is (equal "macro" (type-of-candidate "defu" "defun")))
      (is (equal "special-form" (type-of-candidate "let" "let*")))
      (is (equal "var" (type-of-candidate "*print-cas" "*print-case*")))
      (is (equal "namespace" (type-of-candidate "common-lisp-u" "common-lisp-user"))))))

(test complete-qualified
  (with-connection (c)
    (is (member "cl:mapcar"
                (candidates (send c "op" "completions" "prefix" "cl:mapc"))
                :test #'equal))
    (is (member "nrepl::respond"
                (candidates (send c "op" "completions" "prefix" "nrepl::resp"))
                :test #'equal))
    ;; Single colon: external symbols only.
    (is (not (member "nrepl:respond"
                     (candidates (send c "op" "completions" "prefix" "nrepl:resp"))
                     :test #'equal)))
    (is (member ":test"
                (candidates (send c "op" "completions" "prefix" ":tes"))
                :test #'equal))))

(test complete-in-session-package
  (with-connection (c)
    (is (member "respond"
                (candidates (send c "op" "completions" "prefix" "respo"
                                  "ns" "nrepl"))
                :test #'equal))))

(test complete-nothing
  (with-connection (c)
    (let ((r (send c "op" "completions" "prefix" "zzqqxx-no-such")))
      (is (has-status "done" r))
      (is (null (candidates r))))))
