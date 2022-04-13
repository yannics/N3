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

(defmacro osc-listen (port) 
  #+sbcl
  (let ((s (make-instance 'sb-bsd-sockets::inet-socket 
			  :type :datagram :protocol :udp))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024))
	(var (intern (format nil "*~a*" port))))
    (sb-bsd-sockets::socket-bind s (sb-bsd-sockets::make-inet-address "127.0.0.1") port)
    `(unwind-protect 
	 (loop do
	   (sb-bsd-sockets::socket-receive ,s ,buffer nil)
	   (defparameter ,var (funcall *decode-bundle* ,buffer)))
      (when ,s (sb-bsd-sockets::socket-close ,s))))
  #+openmcl
  (let ((s (ccl:make-socket :local-port port
			    :type :datagram
			    :format :binary))
	(var (intern (format nil "*~a*" port))))
    `(unwind-protect 
	 (loop do
	   (defparameter ,var (funcall *decode-bundle* (ccl:receive-from ,s 2048))))
      (when ,s (close ,s)))))

;; (defparameter listen-port-7771 (ccl:process-run-function "listen-port-7771" #'(lambda () (osc-listen 7771))))
;; (defparameter listen-port-7773 (sb-thread:make-thread #'(lambda () (osc-listen 7773)) :name "listen-port-7773"))
;------------------------------------------------------------------
