(in-package :nrepl)

;;;; Output streams
;;; A character output stream that forwards whatever is written to it back to
;;; the client as responses to a particular message, like:
;;;
;;;   {"out" "...data..."}
;;;
;;; Output is buffered and sent whenever a newline is written or the stream is
;;; flushed, all in the writing thread.  Flushing before responding with a value
;;; or status guarantees the client sees the output first.

(defclass nrepl-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((message :initarg :message :reader stream-message)
   (key :initarg :key :reader stream-key)
   (buffer :initform (make-string-output-stream) :reader stream-buffer)
   (column :initform 0 :accessor stream-column)
   (lock :initform (bt:make-recursive-lock "NREPL output stream lock")
         :reader stream-lock)))

(defun make-nrepl-output-stream (message key)
  "Make a stream that sends its output as `key` in responses to `message`."
  (make-instance 'nrepl-output-stream :message message :key key))

(defun flush-output-stream (stream)
  (bt:with-recursive-lock-held ((stream-lock stream))
    (let ((data (get-output-stream-string (stream-buffer stream))))
      (unless (string= data "")
        (respond (stream-message stream)
                 (make-map (stream-key stream) data))))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream nrepl-output-stream) char)
  (bt:with-recursive-lock-held ((stream-lock stream))
    (write-char char (stream-buffer stream))
    (if (char= char #\Newline)
      (progn (setf (stream-column stream) 0)
             (flush-output-stream stream))
      (incf (stream-column stream))))
  char)

(defmethod trivial-gray-streams:stream-write-string
    ((stream nrepl-output-stream) string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (newline (position #\Newline string :start start :end end
                                             :from-end t)))
    (bt:with-recursive-lock-held ((stream-lock stream))
      (write-string string (stream-buffer stream) :start start :end end)
      (if newline
        (progn (setf (stream-column stream) (- end newline 1))
               (flush-output-stream stream))
        (incf (stream-column stream) (- end start)))))
  string)

(defmethod trivial-gray-streams:stream-line-column
    ((stream nrepl-output-stream))
  (stream-column stream))

(defmethod trivial-gray-streams:stream-force-output
    ((stream nrepl-output-stream))
  (flush-output-stream stream)
  nil)

(defmethod trivial-gray-streams:stream-finish-output
    ((stream nrepl-output-stream))
  (flush-output-stream stream)
  nil)


;;;; Input streams
;;; Each registered session has an input stream that evaluations read
;;; `*standard-input*` from.  When it runs dry the client is sent a
;;; `need-input` status and the reader waits for a `stdin` message to supply
;;; more.  An empty `stdin` message signals end of file.

(defclass nrepl-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((session :initarg :session :reader stream-session)
   (buffer :initform "" :accessor stream-input-buffer)
   (index :initform 0 :accessor stream-input-index)
   (eof :initform nil :accessor stream-input-eof)
   (lock :initform (bt:make-lock "NREPL input stream lock")
         :reader stream-lock)
   (cv :initform (bt:make-condition-variable) :reader stream-cv)))

(defun make-nrepl-input-stream (session)
  (make-instance 'nrepl-input-stream :session session))

(defun add-input (stream string)
  "Make `string` available to readers of `stream`.  An empty string means EOF."
  (bt:with-lock-held ((stream-lock stream))
    (if (string= string "")
      (setf (stream-input-eof stream) t)
      ;; Drop what's been read, except the last character so an unread-char
      ;; (e.g. from peek-char) racing with this still works.
      (let ((start (max 0 (1- (stream-input-index stream)))))
        (setf (stream-input-buffer stream)
              (concatenate 'string
                           (subseq (stream-input-buffer stream) start)
                           string)
              (stream-input-index stream)
              (- (stream-input-index stream) start))))
    (bt:condition-notify (stream-cv stream))))

(defun request-input (stream)
  "Tell the client the evaluation running in `stream`'s session needs input."
  (let ((message (session-running-message (stream-session stream))))
    (when message
      (respond message (make-map "status" '("need-input"))))))

(defun input-available-p (stream)
  (< (stream-input-index stream) (length (stream-input-buffer stream))))

(defmethod trivial-gray-streams:stream-read-char
    ((stream nrepl-input-stream))
  (bt:with-lock-held ((stream-lock stream))
    (let ((requested nil))
      (loop
        (cond
          ((input-available-p stream)
           (return (prog1 (char (stream-input-buffer stream)
                                (stream-input-index stream))
                     (incf (stream-input-index stream)))))
          ((stream-input-eof stream)
           ;; EOF is one-shot, like a terminal: later reads wait for more.
           (setf (stream-input-eof stream) nil)
           (return :eof))
          (t
           (unless requested
             (setf requested t)
             (request-input stream))
           (bt:condition-wait (stream-cv stream) (stream-lock stream))))))))

(defmethod trivial-gray-streams:stream-unread-char
    ((stream nrepl-input-stream) char)
  (declare (ignore char))
  (bt:with-lock-held ((stream-lock stream))
    (decf (stream-input-index stream)))
  nil)

(defmethod trivial-gray-streams:stream-read-char-no-hang
    ((stream nrepl-input-stream))
  (bt:with-lock-held ((stream-lock stream))
    (cond
      ((input-available-p stream)
       (prog1 (char (stream-input-buffer stream) (stream-input-index stream))
         (incf (stream-input-index stream))))
      ((stream-input-eof stream)
       (setf (stream-input-eof stream) nil)
       :eof)
      (t nil))))

(defmethod trivial-gray-streams:stream-listen ((stream nrepl-input-stream))
  (bt:with-lock-held ((stream-lock stream))
    (input-available-p stream)))

(defmethod trivial-gray-streams:stream-clear-input ((stream nrepl-input-stream))
  (bt:with-lock-held ((stream-lock stream))
    (setf (stream-input-buffer stream) ""
          (stream-input-index stream) 0))
  nil)
