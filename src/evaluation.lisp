(in-package :nrepl)

(defvar *last-trace* nil)

(define-condition evaluation-error (error)
  ((text :initarg :text :reader text)
   (orig :initarg :orig :reader orig)
   (data :initarg :data :reader data :initform ())))

(defclass evaluator ()
  ((standard-input :initarg :in :reader in)
   (standard-output :initarg :out :reader out)
   (standard-error :initarg :err :reader err)))


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


(defun evaluate-forms (message forms &key in-package (echo-forms t))
  "Evaluate each form in `forms` and shuttle back the responses.

  `forms` can be a string, in which case the forms will be read out of it one
  at a time (so an `in-package` affects how later forms are read), or a
  ready-to-go list of actual forms.

  Evaluation happens in the current session, whose variables (like `*package*`)
  are saved afterwards.  `in-package` can be a package designator to evaluate
  in, or `nil` to use the session's package.

  If `echo-forms` is true each value response includes the form that
  produced it.

  "
  (let* ((out (make-nrepl-output-stream message "stdout"))
         (err (make-nrepl-output-stream message "stderr"))
         (*standard-output* out)
         (*error-output* err)
         (*trace-output* out)
         (*standard-input* (make-string-input-stream "")))
    (labels ((flush ()
               (finish-output out)
               (finish-output err))
             (eval-form (form)
               (let ((values (nrepl-evaluate-form form)))
                 (update-history form values)
                 (flush)
                 (respond message
                          (with-when
                              (make-map "value" (safe-prin1 (first values)))
                            "form" (when echo-forms (safe-prin1 form))))))
             (eval-all ()
               (if (stringp forms)
                 (with-input-from-string (stream forms)
                   (loop (multiple-value-bind (form found)
                             (read-next-form stream)
                           (if found
                             (eval-form form)
                             (return)))))
                 (mapc #'eval-form forms)))
             (error-respond (e)
               (flush)
               (respond message
                        (apply #'make-map
                               "status" '("error")
                               "error" (text e)
                               "original" (safe-prin1 (orig e))
                               (data e)))))
      (with-session-bindings (*session*)
        (when (and in-package (string/= in-package ""))
          (setf *package* (parse-in-package in-package)))
        (handler-case
            (progn (eval-all)
                   (respond message (make-map "status" '("done"))))
          (evaluation-error (e) (error-respond e)))))))
