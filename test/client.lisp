(in-package :nrepl.test)

;;;; A tiny bencode client for talking to a running server over a real socket.

(defvar *port* nil)
(defvar *timeout* 5)
(defvar *message-id* 0)

(defparameter *terminal-statuses*
  '("done" "error" "session-closed" "unknown-op" "unknown-session"))

(defstruct (connection (:constructor %make-connection))
  socket stream)

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

(defun terminal-p (response)
  (intersection (gethash "status" response) *terminal-statuses*
                :test #'equal))

(defun send (connection &rest keyvals)
  "Send a message built from `keyvals` and return the list of responses to it.

  Responses are collected until one carries a terminal status.

  "
  (let ((id (format nil "~D" (incf *message-id*)))
        (stream (connection-stream connection)))
    (bencode:encode (apply #'make-message "id" id keyvals) stream)
    (force-output stream)
    (loop :for response = (read-response connection)
          :when (equal id (gethash "id" response))
            :collect response :into responses
            :and :do (when (terminal-p response)
                       (return responses)))))


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
