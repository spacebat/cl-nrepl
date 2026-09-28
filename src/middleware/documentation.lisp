(in-package :nrepl)

(defun lambda-list-to-string (lambda-list)
  "Return a single-line string of the lambda list."
  (if (listp lambda-list)
    (format nil "(~{~A~^ ~})" (mapcar #'lambda-list-to-string lambda-list))
    (princ-to-string lambda-list)))

(defun find-lambda-list (symbol)
  "Return the lambda list for the given symbol.

  Will return `nil` if none is found.  A second value is returned to indicate
  whether it was found.

  "
  (if (fboundp symbol)
    (let ((arglist (trivial-arguments:arglist symbol)))
      (if (eql :unknown arglist)
        (values nil nil)
        (values arglist t)))
    (values nil nil)))


(defun parse-symbol-designator-package (string in-package)
  (let ((parts (split-sequence-if (curry #'char= #\:)
                                  (string-upcase string))))
    (case (length parts)
      (0 nil)

      ;; FOO -> ("FOO")
      (1 in-package)

      ;; :FOO -> ("" "FOO")
      ;; P:FOO -> ("P" "FOO")
      (2 (if (string= (first parts) "")
           (find-package "KEYWORD")
           (find-package (first parts))))

      ;; P::FOO -> ("P" "" "FOO")
      (3 (find-package (first parts)))
      (t nil))))

(defun parse-symbol-designator-name (string)
  (let ((parts (split-sequence-if (curry #'char= #\:)
                                  (string-upcase string))))
    (case (length parts)
      (0 nil)

      ;; FOO -> ("FOO")
      (1 (first parts))

      ;; :FOO -> ("" "FOO")
      ;; P:FOO -> ("P" "FOO")
      (2 (second parts))

      ;; P::FOO -> ("P" "" "FOO")
      (3 (third parts))

      (t nil))))

(defun find-symbol-harder (symbol-designator &optional in-package)
  "Return the symbol object with the given `symbol-designator`.

  This should work with names like:

    FOO (looks in the package `in-package`)
    P:FOO (looks in package P)
    P::FOO (looks in package P)
    :FOO (keyword)

  "
  (let ((package (parse-symbol-designator-package symbol-designator in-package))
        (name (parse-symbol-designator-name symbol-designator)))
    (when (and name package)
      (find-symbol name package))))


(define-middleware wrap-documentation "documentation" message
  "Returns the docstrings and arglist of \"symbol\".  Optionally takes \"ns\"."
  (let ((symbol (find-symbol-harder (fset:lookup message "symbol")
                                    (message-package message))))
    (respond message
             (with-when
                 (make-map "status" '("done"))
               "type-docstring" (documentation symbol 'type)
               "structure-docstring" (documentation symbol 'structure)
               "variable-docstring" (documentation symbol 'variable)
               "setf-docstring" (documentation symbol 'setf)
               "function-docstring" (documentation symbol 'function)
               "function-arglist"
               (when-found (arglist (find-lambda-list symbol))
                 (princ-to-string (cons symbol arglist)))))))

(define-middleware wrap-arglist "arglist" message
  "Returns the arglist of \"symbol\".  Optionally takes \"ns\"."
  (let ((symbol (find-symbol-harder (fset:lookup message "symbol")
                                    (message-package message))))
    (respond message
             (with-when
                 (make-map "status" '("done"))
               "function-arglist"
               (when-found (arglist (find-lambda-list symbol))
                 (lambda-list-to-string (cons symbol arglist)))))))


;;;; Lookup
(defun symbol-kind (symbol)
  "Return the nREPL type of the thing `symbol` names, or `nil`."
  (cond ((keywordp symbol) "keyword")
        ((special-operator-p symbol) "special-form")
        ((macro-function symbol) "macro")
        ((fboundp symbol) "function")
        ((boundp symbol) "var")
        ((find-class symbol nil) "class")
        (t nil)))

(defun symbol-docstring (symbol)
  (or (documentation symbol 'function)
      (documentation symbol 'variable)
      (documentation symbol 'type)
      (documentation symbol 'structure)))

(defun arglist-string (symbol)
  "Return `symbol`'s lambda list as a string like \"(x &optional y)\"."
  (when-found (arglist (find-lambda-list symbol))
    (let ((*print-case* :downcase)
          (*package* (symbol-package symbol)))
      (lambda-list-to-string arglist))))

(defun symbol-info (symbol)
  (with-when
      (make-map "name" (lisp-name (symbol-name symbol)))
    "ns" (when (symbol-package symbol)
           (package-name (symbol-package symbol)))
    "type" (symbol-kind symbol)
    "arglists-str" (arglist-string symbol)
    "doc" (symbol-docstring symbol)))

(define-middleware wrap-lookup "lookup" message
  "Looks up information about the symbol \"sym\": its \"name\", \"ns\",
  \"type\", \"arglists-str\" and \"doc\".  Optionally takes \"ns\".
  Returns an empty \"info\" if there's no such symbol."
  (let* ((name (or (fset:lookup message "sym") (fset:lookup message "symbol")))
         (symbol (and name (find-symbol-harder name (message-package message)))))
    (respond message
             (make-map "status" '("done")
                       "info" (if (and symbol (symbol-kind symbol))
                                (symbol-info symbol)
                                (fset:empty-map))))))


;;;; Completion
(defun prefix-match-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun symbol-candidate (symbol text)
  (with-when
      (make-map "candidate" text
                "type" (symbol-kind symbol))
    "ns" (when (symbol-package symbol)
           (package-name (symbol-package symbol)))))

(defun complete-symbols (prefix package &key (qualifier "") external-only)
  "Return candidates for symbols in `package` whose names start with `prefix`.

  Only symbols that name something (a function, variable, class...) are
  included.  Each candidate's text is `qualifier` followed by the name.

  "
  (let ((seen (make-hash-table :test #'eq))
        (results '()))
    (flet ((consider (symbol)
             (when (and (not (gethash symbol seen))
                        (prefix-match-p prefix (symbol-name symbol))
                        (symbol-kind symbol))
               (setf (gethash symbol seen) t)
               (push (symbol-candidate
                       symbol
                       (concatenate 'string qualifier
                                    (lisp-name (symbol-name symbol))))
                     results))))
      (if external-only
        (do-external-symbols (s package) (consider s))
        (do-symbols (s package) (consider s))))
    results))

(defun complete-packages (prefix)
  (loop :for package :in (list-all-packages)
        :append (loop :for name :in (cons (package-name package)
                                          (package-nicknames package))
                      :when (prefix-match-p prefix name)
                        :collect (make-map "candidate" (lisp-name name)
                                           "type" "namespace"))))

(defun completions (prefix package)
  "Return completion candidates for `prefix`, typed in `package`.

  Handles plain symbols (and package names), `package:symbol`,
  `package::symbol` and `:keyword`.

  "
  (let ((colon (position #\: prefix)))
    (if (null colon)
      (append (complete-symbols prefix package)
              (complete-packages prefix))
      (let* ((package-part (subseq prefix 0 colon))
             (internal (and (< (1+ colon) (length prefix))
                            (char= #\: (char prefix (1+ colon)))))
             (name-part (subseq prefix (+ colon (if internal 2 1))))
             (target (if (string= package-part "")
                       (find-package :keyword)
                       (find-package-named package-part))))
        (when target
          (complete-symbols name-part target
                            :qualifier (subseq prefix 0 (+ colon (if internal 2 1)))
                            :external-only (not internal)))))))

(define-middleware wrap-completions "completions" message
  "Returns completion candidates for \"prefix\" as \"completions\", each with a
  \"candidate\", \"type\" and (for symbols) \"ns\".  Optionally takes \"ns\"."
  (respond message
           (make-map "status" '("done")
                     "completions"
                     (sort (completions (or (fset:lookup message "prefix") "")
                                        (message-package message))
                           #'string<
                           :key (lambda (c) (fset:lookup c "candidate"))))))
