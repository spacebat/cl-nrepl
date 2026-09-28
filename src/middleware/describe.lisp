(in-package :nrepl)

(defun parse-version (string)
  "Return the first three integers in `string`, padding with 0s."
  (let ((numbers '())
        (start 0))
    (loop :for digit = (position-if #'digit-char-p string :start start)
          :while (and digit (< (length numbers) 3))
          :do (multiple-value-bind (n end)
                  (parse-integer string :start digit :junk-allowed t)
                (push n numbers)
                (setf start end)))
    (let ((numbers (reverse numbers)))
      (append numbers (make-list (- 3 (length numbers)) :initial-element 0)))))

(defun make-version-map (version-string)
  (destructuring-bind (major minor incremental) (parse-version version-string)
    (make-map "major" major
              "minor" minor
              "incremental" incremental
              "version-string" version-string)))

(defun nrepl-version ()
  (or (ignore-errors (asdf:component-version (asdf:find-system :nrepl)))
      "0.0.0"))

(defun describe-ops (verbose)
  (fset:convert 'fset:map
                (loop :for op :being :the :hash-keys :of *ops*
                      :using (hash-value doc)
                      :collect (cons op (if (and verbose (string/= doc ""))
                                          (make-map "doc" doc)
                                          (fset:empty-map))))))

(define-middleware wrap-describe "describe" message
  "Describes the server: the ops it supports (with docs if \"verbose?\" is
  given), versions, and the session's current package in \"aux\"."
  (respond message
           (make-map "status" '("done")
                     "versions" (make-map
                                  "lisp" (with-when
                                             (make-version-map
                                               (lisp-implementation-version))
                                           "implementation"
                                           (lisp-implementation-type))
                                  "cl-nrepl" (make-version-map (nrepl-version)))
                     "ops" (describe-ops (fset:lookup message "verbose?"))
                     "aux" (make-map "current-ns"
                                     (package-name (session-package))))))
