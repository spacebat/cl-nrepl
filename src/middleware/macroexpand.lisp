(in-package :nrepl)

(defun pretty-string (form)
  "Return a prettified string version of `form`, indented nicely."
  (with-output-to-string (*standard-output*)
    (let ((*print-pretty* t)
          (*print-escape* t))
      (write form))))

(define-middleware wrap-macroexpand "macroexpand" message
  (let ((*package* (parse-in-package (fset:lookup message "in-package"))))
    (multiple-value-bind (form readp)
        (handler-case (values (read-from-string (fset:lookup message "form")) t)
          (error () (values nil nil)))
      (respond message
               (if readp
                 (make-map
                   "status" '("done")
                   "macroexpand" (pretty-string (macroexpand form))
                   "macroexpand-1" (pretty-string (macroexpand-1 form)))
                 (make-map "status" '("error")
                           "error" "Malformed input!"))))))
