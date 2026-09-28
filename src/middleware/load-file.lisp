(in-package :nrepl)

(defun load-from-string (contents &optional file-name)
  "`load` the forms in the string `contents`.

  As with `load`, changes to `*package*` made by the file don't leak out.

  "
  (with-input-from-string (stream contents)
    (let ((*load-pathname* (when file-name (pathname file-name)))
          (*load-truename* nil))
      (load stream))))

(define-middleware wrap-load-file "load-file" message
  ;; The standard nREPL op sends the file's contents as "file".  Loading a
  ;; server-side "path" is kept for backwards compatibility.
  (let ((contents (fset:lookup message "file"))
        (file-name (fset:lookup message "file-name"))
        (path (fset:lookup message "path")))
    (evaluate-forms message
                    (list (if contents
                            `(load-from-string ,contents ,file-name)
                            `(load ,path)))
                    :echo-forms nil)))
