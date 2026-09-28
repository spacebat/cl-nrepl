(in-package :nrepl)

;;;; Sessions
;;; A session holds the values of the REPL's special variables between
;;; evaluations, so things like `(in-package ...)` and `*` stick around from one
;;; request to the next.
;;;
;;; Registered sessions (made by `clone`) run their evaluations one at a time
;;; on a worker thread of their own.  That leaves the connection free to read
;;; further messages while code runs, which `interrupt` and `stdin` need.
;;; Ephemeral sessions (for messages that don't name a session) evaluate
;;; directly on the connection's thread.

(defparameter *session-variables*
  '(*package* * ** *** + ++ +++ / // ///)
  "Special variables whose values are saved in each session.")

(defstruct (session (:constructor %make-session))
  (id (random-uuid) :type string)
  (bindings nil :type list)
  (lock (bt:make-lock "NREPL session lock"))
  (ephemeral nil)
  (input nil)
  ;; Work queue, guarded by queue-lock.
  (queue nil :type list)
  (queue-lock (bt:make-lock "NREPL session queue lock"))
  (queue-cv (bt:make-condition-variable))
  (worker nil)
  (closed nil)
  ;; The message being evaluated and the catch tag that interrupts it, also
  ;; guarded by queue-lock.
  (running-message nil)
  (running-tag nil))

(defun default-session-bindings ()
  (loop :for var :in *session-variables*
        :collect (cons var (if (eq var '*package*)
                             (find-package :cl-user)
                             nil))))

(defun make-session (&key parent ephemeral)
  "Make a new session, copying the state of `parent` if given."
  ;; No need for the parent's lock: its bindings are only ever replaced
  ;; wholesale, never mutated, and taking it would block cloning a session that's
  ;; busy evaluating.
  (let ((session (%make-session
                   :ephemeral ephemeral
                   :bindings (if parent
                               (copy-alist (session-bindings parent))
                               (default-session-bindings)))))
    (unless ephemeral
      (setf (session-input session) (make-nrepl-input-stream session)))
    session))

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


;;;; Work queue
(defun session-worker-loop (session)
  (loop
    (let ((job (bt:with-lock-held ((session-queue-lock session))
                 (loop :until (or (session-queue session)
                                  (session-closed session))
                       :do (bt:condition-wait (session-queue-cv session)
                                              (session-queue-lock session)))
                 (if (session-closed session)
                   (return)
                   (pop (session-queue session))))))
      (funcall job))))

(defun session-submit (session thunk)
  "Queue `thunk` to be run on `session`'s worker thread, after anything already
  queued."
  (bt:with-lock-held ((session-queue-lock session))
    (setf (session-queue session)
          (append (session-queue session) (list thunk)))
    (unless (and (session-worker session)
                 (bt:thread-alive-p (session-worker session)))
      (setf (session-worker session)
            (bt:make-thread (lambda () (session-worker-loop session))
                            :name (format nil "NREPL session ~A"
                                          (session-id session)))))
    (bt:condition-notify (session-queue-cv session))))


;;;; Interruption
(defmacro with-interruptible-evaluation ((session message) &body body)
  "Run `body` so that `interrupt-session` can abort it.

  Returns the values of `body`, or `:interrupted` if it was interrupted.

  "
  (with-gensyms (s tag)
    `(let ((,s ,session)
           (,tag (list :interrupt)))
      (catch ,tag
        (unwind-protect
            (progn
              (bt:with-lock-held ((session-queue-lock ,s))
                (setf (session-running-message ,s) ,message
                      (session-running-tag ,s) ,tag))
              ,@body)
          ;; Clear the tag with interrupts deferred: once it's cleared no
          ;; interrupt will throw to it, and until then we're still inside the
          ;; catch, so a throw always has somewhere to land.
          (without-interrupts
            (bt:with-lock-held ((session-queue-lock ,s))
              (setf (session-running-message ,s) nil
                    (session-running-tag ,s) nil))))))))

(defun interrupt-session (session &optional interrupt-id)
  "Interrupt the evaluation currently running in `session`.

  Returns `:interrupted`, `:idle` if nothing is running, or `:mismatch` if
  `interrupt-id` is given and the running message has a different id.

  "
  (bt:with-lock-held ((session-queue-lock session))
    (let ((message (session-running-message session))
          (tag (session-running-tag session)))
      (cond
        ((null message) :idle)
        ((and interrupt-id
              (not (equal interrupt-id (fset:lookup message "id"))))
         :mismatch)
        (t (bt:interrupt-thread
             (session-worker session)
             (lambda ()
               ;; This runs in the worker thread, possibly after the evaluation
               ;; finished, so only throw if it's still the one we meant.
               (when (eq tag (session-running-tag session))
                 (throw tag :interrupted))))
           :interrupted)))))

(defun close-session (session)
  "Stop `session`: interrupt anything running and let its worker exit."
  (interrupt-session session)
  (bt:with-lock-held ((session-queue-lock session))
    (setf (session-closed session) t
          (session-queue session) nil)
    (bt:condition-notify (session-queue-cv session))))


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

(defun find-package-named (name)
  "Find the package called `name`, as typed by a user or sent by a client.

  Accepts names in any case, with or without a leading `:` or `#:`.

  "
  (let ((name (string-left-trim ":" (if (and (> (length name) 1)
                                             (string= "#:" name :end2 2))
                                      (subseq name 1)
                                      name))))
    (or (find-package name)
        (find-package (string-upcase name)))))

(defun parse-in-package (in-package)
  "Return the package designated by the string `in-package`.

  Falls back to the current session's package if `in-package` is empty or
  doesn't name a package.

  "
  (or (and (stringp in-package)
           (string/= in-package "")
           (find-package-named in-package))
      (session-package)))

(defun message-package (message)
  "Return the package `message` asks to work in, falling back to the session's.

  The standard field is \"ns\"; \"in-package\" is accepted too.

  "
  (parse-in-package (or (fset:lookup message "ns")
                        (fset:lookup message "in-package"))))
