(in-package :nrepl.test)

;;;; A tiny bencode client for talking to a running server over a real socket.

(defvar *port* nil)
(defvar *timeout* 5)
(defvar *message-id* 0)

(defstruct (connection (:constructor %make-connection))
  socket
  stream
  ;; Responses read while waiting for something else, oldest first.
  (inbox nil))

(defun connect (&optional (port *port*))
  (let ((socket (usocket:socket-connect "127.0.0.1" port
                                        :element-type '(unsigned-byte 8))))
    (%make-connection
      :socket socket
      :stream (flex:make-flexi-stream (usocket:socket-stream socket)
                                      :external-format :utf-8))))

(defun disconnect (connection)
  (usocket:socket-close (connection-socket connection)))

(defmacro with-connection ((var) &body body)
  `(let ((,var (connect)))
    (unwind-protect (progn ,@body)
      (disconnect ,var))))

(defun make-message (&rest keyvals)
  (let ((h (make-hash-table :test #'equal)))
    (loop :for (k v) :on keyvals :by #'cddr
          :when v :do (setf (gethash k h) v))
    h))

(defun wait-for-response (connection)
  "Wait until a response can be read from `connection`, or signal an error.

  Several responses can arrive in one packet, so the stream's buffer is checked
  before waiting on the socket, which would block with the data already read.
  The wait can also return early (e.g. when interrupted by a signal), so keep
  waiting until the deadline has really passed.

  "
  (let ((deadline (+ (get-internal-real-time)
                     (* *timeout* internal-time-units-per-second))))
    (loop
      (when (listen (connection-stream connection))
        (return))
      (let ((remaining (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second)))
        (when (<= remaining 0)
          (error "Timed out waiting for a response from the server."))
        (usocket:wait-for-input (connection-socket connection)
                                :timeout remaining :ready-only t)))))

(defun read-response (connection)
  (wait-for-response connection)
  (bencode:decode (connection-stream connection)))

(defun has-status-p (status response)
  (member status (gethash "status" response) :test #'equal))

(defun send-message (connection &rest keyvals)
  "Send a message built from `keyvals` without waiting.  Returns its id."
  (let ((id (format nil "~D" (incf *message-id*)))
        (stream (connection-stream connection)))
    (bencode:encode (apply #'make-message "id" id keyvals) stream)
    (force-output stream)
    id))

(defun next-response (connection id)
  "Return the next response to the message with `id`.

  Responses to other messages that arrive in the meantime are kept for later.

  "
  (let ((stashed (find id (connection-inbox connection)
                       :key (lambda (r) (gethash "id" r)) :test #'equal)))
    (if stashed
      (progn (setf (connection-inbox connection)
                   (remove stashed (connection-inbox connection) :count 1))
             stashed)
      (loop :for response = (read-response connection)
            :if (equal id (gethash "id" response))
              :return response
            :else
              :do (setf (connection-inbox connection)
                        (append (connection-inbox connection)
                                (list response)))))))

(defun responses-until (connection id status)
  "Return the responses to message `id` up to and including one with `status`."
  (loop :for response = (next-response connection id)
        :collect response
        :until (has-status-p status response)))

(defun responses (connection id)
  "Return all the responses to message `id`, up to its \"done\"."
  (responses-until connection id "done"))

(defun send (connection &rest keyvals)
  "Send a message built from `keyvals` and return the list of responses to it."
  (responses connection (apply #'send-message connection keyvals)))


;;;; Response helpers
(defun field (key responses)
  "Return every value of `key` across `responses`, in order."
  (loop :for r :in responses
        :for (v found) = (multiple-value-list (gethash key r))
        :when found :collect v))

(defun joined (key responses)
  (format nil "~{~A~}" (field key responses)))

(defun statuses (responses)
  (reduce #'append (field "status" responses)))

(defun value (responses)
  (car (last (field "value" responses))))

(defun position-of (pred responses)
  (position-if pred responses))
