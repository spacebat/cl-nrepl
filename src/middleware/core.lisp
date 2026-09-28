(in-package :nrepl)

(defvar *ops* (make-hash-table :test #'equal)
  "Map of op names to their docstrings, for `describe`.")


(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (fset:lookup ,message "op"))
    (progn ,@body)
    (funcall ,fallback ,message)))

(defmacro define-middleware (name op message-binding &body body)
  "Define a middleware at the symbol `name` to handle `op`.

  As the body is executing `message-binding` will be bound to the message map.
  If the body starts with a docstring (and has more after it) it's used to
  describe the op in response to `describe`.

  "
  (let ((doc (when (and (stringp (first body)) (rest body))
               (pop body))))
    (with-gensyms (fallback)
      `(progn
        (setf (gethash ,op *ops*) ,(or doc ""))
        (defun ,name (,fallback)
          (lambda (,message-binding)
            (handle-op ,message-binding ,op ,fallback
                       ,@body)))))))
