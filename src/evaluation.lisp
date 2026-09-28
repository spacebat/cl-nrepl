(in-package :nrepl)

(defvar *last-trace* nil)

(define-condition evaluation-error (error)
  ((text :initarg :text :reader text)
   (orig :initarg :orig :reader orig)
   (data :initarg :data :reader data :initform ())))

(defun read-next-form (stream)
  "Read the next form from `stream`, returning `(values form t)`.

  Returns `(values nil nil)` once only whitespace and comments remain.
  Signals an `evaluation-error` if the input is mangled.

  "
  (let ((eof (load-time-value (make-symbol "EOF"))))
    (handler-case
        (let ((form (read stream nil eof)))
          (if (eq form eof)
            (values nil nil)
            (values form t)))
      (error (e)
        (error 'evaluation-error
               :text "Malformed input!"
               :orig e)))))

(defun safe-prin1 (object)
  "Return `object` printed with `prin1`, without ever signaling."
  (handler-case (prin1-to-string object)
    (error (e)
      (format nil "#<error printing object: ~A>" (safe-princ e)))))


(defun parse-frame (frame)
  (let ((call-form (list* (dissect:call frame) (dissect:args frame)))
        (location (list (dissect:file frame) (dissect:line frame))))
    (list call-form location)))

(defun parse-stack (stack)
  (mapcar #'parse-frame (nthcdr 1 (reverse stack))))

(defun string-trace-whole (trace)
  (with-output-to-string (s)
    (loop :for (call-form (file line)) :in trace
          :do (format s "~S~%" call-form))))

(defun string-trace-components (trace)
  (loop :for (call-form (file line)) :in trace
        :collect (list (format nil "~S" call-form)
                       (if file
                         (format nil "~A" file)
                         "")
                       (if line
                         (format nil "~D" line)
                         ""))))


(defun nrepl-evaluate-form (form)
  "Evaluate `form` and return a list of its values."
  (declare (optimize (debug 3)))
  (handler-bind
    ((error
       (lambda (err)
         ; if we hit an error, get the stack trace before reraising.  if we
         ; wait til later to print it, it'll be too late.
         (error 'evaluation-error
                :text "Error during evaluation!"
                :orig err
                :data (let* ((raw (dissect:stack))
                             (clean (parse-stack raw)))
                        (setf *last-trace* raw)
                        (list
                          "form" (safe-prin1 form)
                          "stack-trace" (string-trace-components clean)
                          "stack-trace-string" (string-trace-whole clean)))))))
    (dissect:with-truncated-stack ()
      (multiple-value-list (eval form)))))

(defun update-history (form values)
  "Update the REPL history variables the way the standard REPL does."
  (setf +++ ++ ++ + + form
        /// // // / / values
        *** ** ** * * (first values)))

(defun print-values (values)
  "Print `values` for a \"value\" response: one per line, like the REPL."
  (if values
    (format nil "~{~A~^~%~}" (mapcar #'safe-prin1 values))
    "; No values"))


(defun report-evaluation-error (message e out err)
  "Report the `evaluation-error` `e` the way nREPL does: a description on
  stderr, then an `eval-error` status naming the condition's type."
  (let ((condition (orig e)))
    (format err "~A~@[ (~A)~]:~%~A~%"
            (text e)
            (when condition (qualified-name (type-of condition)))
            (if condition (safe-princ condition) ""))
    (finish-output out)
    (finish-output err)
    (let ((type (qualified-name (type-of (or condition e)))))
      (respond message
               (apply #'make-map
                      "status" '("eval-error")
                      "ex" type
                      "root-ex" type
                      (data e))))))

(defun evaluate-forms (message forms &key ns)
  "Evaluate each form in `forms` and send back the responses.

  `forms` can be a string, in which case the forms will be read out of it one
  at a time (so an `in-package` affects how later forms are read), or a
  ready-to-go list of actual forms.

  Evaluation happens in the current session, whose variables (like `*package*`)
  are saved afterwards.  `ns` can name a package to evaluate in, or be `nil` to
  use the session's package.

  Responses follow nREPL: output as \"out\"/\"err\", a \"value\" and \"ns\"
  for each form, an \"eval-error\" status (with \"ex\") for each form that
  signals an error, and finally a \"done\" status.  As in nREPL, an error
  reading the code stops evaluation but an error evaluating a form doesn't.

  "
  (let* ((session *session*)
         (out (make-nrepl-output-stream message "out"))
         (err (make-nrepl-output-stream message "err"))
         (in (or (session-input session) (make-string-input-stream "")))
         (*standard-output* out)
         (*error-output* err)
         (*trace-output* out)
         (*standard-input* in)
         (*query-io* (make-two-way-stream in out)))
    (labels ((flush ()
               (finish-output out)
               (finish-output err))
             (eval-form (form)
               (handler-case
                   (let ((values (nrepl-evaluate-form form)))
                     (update-history form values)
                     (flush)
                     (respond message
                              (make-map "value" (print-values values)
                                        "ns" (package-name *package*))))
                 (evaluation-error (e)
                   (report-evaluation-error message e out err))))
             (eval-all ()
               (if (stringp forms)
                 (with-input-from-string (stream forms)
                   (loop (multiple-value-bind (form found)
                             (read-next-form stream)
                           (if found
                             (eval-form form)
                             (return)))))
                 (mapc #'eval-form forms))))
      (with-session-bindings (session)
        (when ns
          (setf *package* (parse-in-package ns)))
        (let ((result (with-interruptible-evaluation (session message)
                        (handler-case (progn (eval-all) nil)
                          ;; Only reading can signal this here: eval-form
                          ;; handles its own.
                          (evaluation-error (e)
                            (report-evaluation-error message e out err))))))
          (flush)
          (respond message
                   (make-map "status" (if (eq result :interrupted)
                                        '("done" "interrupted")
                                        '("done")))))))))
