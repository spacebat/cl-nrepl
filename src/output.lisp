(in-package :nrepl)

;;;; Output streams
;;; A character output stream that forwards whatever is written to it back to
;;; the client as responses to a particular message, like:
;;;
;;;   {"status" ["ok"] "stdout" "...data..."}
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
                 (make-map "status" '("ok")
                           (stream-key stream) data))))))

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
