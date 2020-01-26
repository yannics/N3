;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                     SEND-PORT/UDP

(defvar *encode-message* nil)
(defvar *decode-bundle* nil)
(cond ((find-package 'fosc) (format t "loaded: FOSC~%") (setf *encode-message* (find-symbol "ENCODE-MESSAGE" "FOSC") *decode-bundle* (find-symbol "DECODE-BUNDLE" "FOSC")))
      ((find-package 'osc) (format t "loaded: OSC~%") (setf *encode-message* (find-symbol "ENCODE-MESSAGE" "OSC") *decode-bundle* (find-symbol "DECODE-BUNDLE" "OSC")))
      (t (warn "The package FOSC or OSC is required to enable UDP.")))

(defun send-udp (message host port)
  #+sbcl
  (let ((s (make-instance 'sb-bsd-sockets::inet-socket
			  :type :datagram :protocol :udp)))
    (sb-bsd-sockets::socket-connect s (sb-bsd-sockets::make-inet-address host) port)
    (sb-bsd-sockets::socket-send s (apply *encode-message* message) nil)
    (sb-bsd-sockets::socket-close s))
  #+openmcl
  (let* ((temp-buffer (apply *encode-message* message))
	 (out-vector (make-array (length temp-buffer) 
				 :element-type'(unsigned-byte 8)
				 :initial-contents temp-buffer))
	 (s (ccl:make-socket :type :datagram )))
    (ccl:send-to s out-vector (length out-vector) :remote-host host :remote-port port)
    (ccl::close s)))

;; (send-udp (read-from-string (format nil "(\"/~A\" ~{\"~S\"~})" 'N3 (list 1 2 3))) "127.0.0.1" 7771)
;------------------------------------------------------------------
;                                                   LISTEN-PORT/UDP

(defun int-char-p (c)
  (if (find c ". 1 2 3 4 5 6 7 8 9 0 e E + -") t nil))

(defun str-to-list (str)
  (if (not (streamp str))
      (let ((rrr (ignore-errors (str-to-list (make-string-input-stream str)))))
	(when rrr rrr))
      (if (listen str)
	  (cons (read str) (str-to-list str))
	  nil)))

(defun read-string-msg (str)
  "list only number from string"
  (loop for n in (str-to-list (coerce (loop for i in (concatenate 'list (substitute #\space #\, str)) when (or (eq " " (string i)) (int-char-p i)) collect i) 'string)) when (numberp n) collect n))

(defvar *data-buffer* nil)

(defun osc-listen (port) 
  #+sbcl
  (let ((s (make-instance 'sb-bsd-sockets::inet-socket 
              :type :datagram :protocol :udp))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (sb-bsd-sockets::socket-bind s (sb-bsd-sockets::make-inet-address "127.0.0.1") port)
    (unwind-protect 
     (loop do
	  (sb-bsd-sockets::socket-receive s buffer nil)    
	  (push (let ((msg (funcall *decode-bundle* buffer)))
		  ;(list (car msg) (read-string-msg (cadr msg)))
		  msg) *data-buffer*))
      (when s (sb-bsd-sockets::socket-close s))))
  #+openmcl
  (let ((s (ccl:make-socket :local-port port
			    :type :datagram
			    :format :binary)))
    (unwind-protect 
	 (loop do
	      (push (let ((msg (funcall *decode-bundle* (ccl:receive-from s 2048))))
		      ;(list (car msg) (read-string-msg (cadr msg)))
		      msg) *data-buffer*))
      (when s (close S)))))

;; (setf listen-port-7771 (ccl:process-run-function "listen-port-7771" #'(lambda () (osc-listen 7771))))
;; (setf listen-port-7773 (sb-thread:make-thread #'(lambda () (osc-listen 7773)) :name "listen-port-7773"))
;------------------------------------------------------------------

