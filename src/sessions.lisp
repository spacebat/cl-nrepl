(in-package :nrepl)

;;;; Sessions
;;; A session holds the values of the REPL's special variables between
;;; evaluations, so things like `(in-package ...)` and `*` stick around from one
;;; request to the next.

(defparameter *session-variables*
  '(*package* * ** *** + ++ +++ / // ///)
  "Special variables whose values are saved in each session.")

(defstruct (session (:constructor %make-session))
  (id (random-uuid) :type string)
  (bindings nil :type list)
  (lock (bt:make-lock "NREPL session lock")))

(defun default-session-bindings ()
  (loop :for var :in *session-variables*
        :collect (cons var (if (eq var '*package*)
                             (find-package :cl-user)
                             nil))))

(defun make-session (&optional parent)
  "Make a new session, copying the state of `parent` if given."
  ;; No need for the parent's lock: its bindings are only ever replaced
  ;; wholesale, never mutated, and taking it would block cloning a session that's
  ;; busy evaluating.
  (%make-session
    :bindings (if parent
                (copy-alist (session-bindings parent))
                (default-session-bindings))))

(defun session-value (session var)
  (cdr (assoc var (session-bindings session))))

(defmacro with-session-bindings ((session) &body body)
  "Run `body` with `session`'s variables bound, saving them back afterwards.

  Holds the session's lock for the duration, so evaluations within a single
  session are serialized.

  "
  (with-gensyms (s bindings)
    `(let ((,s ,session))
      (bt:with-lock-held ((session-lock ,s))
        (let ((,bindings (session-bindings ,s)))
          (progv (mapcar #'car ,bindings) (mapcar #'cdr ,bindings)
            (unwind-protect (progn ,@body)
              (setf (session-bindings ,s)
                    (loop :for (var) :in ,bindings
                          :collect (cons var (symbol-value var)))))))))))


;;;; Registry
(defvar *sessions* (make-hash-table :test #'equal))
(defvar *sessions-lock* (bt:make-lock "NREPL session registry lock"))
(defvar *session* nil
  "The session the current message is being handled in.")

(defun clear-sessions! ()
  (bt:with-lock-held (*sessions-lock*)
    (clrhash *sessions*)))

(defun register-session! (session)
  (bt:with-lock-held (*sessions-lock*)
    (setf (gethash (session-id session) *sessions*) session)))

(defun remove-session! (id)
  (bt:with-lock-held (*sessions-lock*)
    (remhash id *sessions*)))

(defun get-session (id)
  (bt:with-lock-held (*sessions-lock*)
    (gethash id *sessions*)))

(defun get-sessions ()
  (bt:with-lock-held (*sessions-lock*)
    (hash-table-keys *sessions*)))


;;;; Packages
(defun session-package ()
  "Return the package of the current session, or `*package*` if there is none."
  (if *session*
    (session-value *session* '*package*)
    *package*))

(defun parse-in-package (in-package)
  "Return the package designated by the string `in-package`.

  Falls back to the current session's package if `in-package` is empty or
  doesn't name a package.

  "
  (or (and in-package
           (string/= in-package "")
           (let ((designator (ignore-errors
                               (let ((*read-eval* nil))
                                 (read-from-string in-package)))))
             (and (typep designator '(or symbol string))
                  (find-package designator))))
      (session-package)))
