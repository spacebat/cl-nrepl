(in-package :nrepl)

(defun load-from-string (contents &optional file-name)
  "Load the forms in the string `contents`, returning the last form's values.

  As with `load`, changes to `*package*` and `*readtable*` made by the file
  don't leak out.

  "
  (let ((*package* *package*)
        (*readtable* *readtable*)
        (*load-pathname* (when file-name (pathname file-name)))
        (*load-truename* nil)
        (eof (load-time-value (make-symbol "EOF")))
        (values '(nil)))
    (with-input-from-string (stream contents)
      (loop :for form = (read stream nil eof)
            :until (eq form eof)
            :do (setf values (multiple-value-list (eval form)))))
    (values-list values)))

(define-middleware wrap-load-file "load-file" message
  "Loads a file's contents.  Requires \"file\" (the contents); optionally
  \"file-name\" and \"file-path\".  Returns the value of the last form."
  ;; The standard nREPL op sends the file's contents as "file".  Loading a
  ;; server-side "path" is kept for backwards compatibility.
  (let ((contents (fset:lookup message "file"))
        (file-name (fset:lookup message "file-name"))
        (path (fset:lookup message "path")))
    (run-evaluation message
                    (lambda ()
                      (evaluate-forms message
                                      (list (if contents
                                              `(load-from-string ,contents ,file-name)
                                              `(load ,path))))))))
