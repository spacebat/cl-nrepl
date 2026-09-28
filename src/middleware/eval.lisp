(in-package :nrepl)

(define-middleware wrap-eval "eval" message
  (evaluate-forms message
                  (fset:lookup message "code")
                  :in-package (fset:lookup message "in-package")))
