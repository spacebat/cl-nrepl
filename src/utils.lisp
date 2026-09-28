(in-package :nrepl)

(defvar *log* *error-output*)


(defun make-map (&rest keyvals)
  "Create an fset map of the given riffle of keys and values."
  (fset:convert 'fset:map (loop :for (k v) :on keyvals :by #'cddr
                                :collect (cons k v))))

(defun with-when (map &rest keyvals)
  "Add the items in the `keyvals` riffle with non-nil values to `map`."
  (labels ((build (map keyvals)
             (if (null keyvals)
               map
               (destructuring-bind (k v &rest remaining) keyvals
                 (build (if v
                          (fset:with map k v)
                          map)
                        remaining)))))
    (build map keyvals)))

(defun random-uuid ()
  "Return a random UUID as a string."
  (format nil "~a" (uuid:make-v4-uuid)))

(defun safe-princ (object)
  "Return `object` printed with `princ`, without ever signaling."
  (handler-case (princ-to-string object)
    (error () (format nil "#<~S>" (type-of object)))))

(defun log-message (&rest args)
  (apply #'format *log* args)
  (force-output *log*))

(defun respond (message response)
  "Respond to `message` with the `response` map.

  Takes care of finding the transport and patching the message and session IDs
  into the response.

  "
  (funcall (fset:lookup message "transport")
           (with-when response
             "id" (fset:lookup message "id")
             "session" (fset:lookup message "session"))))


(defun call-reporting-errors (message thunk)
  "Call `thunk`, responding to `message` with an error instead of signaling.

  An error escaping from a message handler would take down the thread handling
  it (or, with the debugger enabled, hang it), so report it to the client.

  "
  (handler-case (funcall thunk)
    (serious-condition (c)
      (log-message "~%; Error handling message: ~A~%" c)
      (ignore-errors
        (respond message
                 (make-map "status" '("done" "error")
                           "error" (safe-princ c)))))))


(defmacro without-interrupts (&body body)
  "Run `body` with interrupts from `bt:interrupt-thread` deferred, if the
  implementation supports it."
  #+sbcl `(sb-sys:without-interrupts ,@body)
  #+ccl `(ccl:without-interrupts ,@body)
  #-(or sbcl ccl) `(progn ,@body))


(defun lisp-name (string)
  "Return `string` (a symbol or package name) the way a user would type it.

  Names with no lowercase characters are downcased, since the reader upcases
  what's typed.  Anything else is left as-is.

  "
  (if (string= string (string-upcase string))
    (string-downcase string)
    string))

(defun qualified-name (symbol)
  "Return the fully package-qualified name of `symbol` as a string."
  (let ((*package* (find-package :keyword)))
    (prin1-to-string symbol)))


(defmacro when-found ((var lookup-expr) &body body)
  "Perform `body` with `var` bound to the results of `lookup-expr`, when valid.

  `lookup-expr` should be an expression that returns two values, the first being
  the result (which will be bound to `var`) and the second indicating whether
  the lookup was successful.  The standard `gethash` is an example of a function
  that behaves like this.

  Instead of:
  (multiple-value-bind (val found) (gethash :foo hash)
    (when found
      body))

  (when-found (val (gethash :foo hash))
    body)

  "
  (with-gensyms (found)
    `(multiple-value-bind (,var ,found) ,lookup-expr
      (when ,found
        ,@body))))
