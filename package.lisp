(defpackage :N3
  (:use :cl)
  (:export :*N3-BACKUP-DIRECTORY* 
	   :*ALL-SOM*
	   :*ALL-AREA*
	   :CREATE-MLT
	   :CREATE-AREA
	   :LOCATE-CLIQUE
	   :LOCATE-TOURNOI
	   :LOCATE-CYCLE
	   :UPDATE-FANAUX
	   :UPDATE-COVER-VALUE
	   :LEARN
	   :SAVE
	   :COPY
	   :LOAD-NEURAL-NETWORK
	   :NEXT-EVENT-PROBABILITY
	  
	   :*TREE*
	   :GET-LEAVES
	   :CAH-FANAUX
	   :DENDROGRAM

	   :NET-MENU
	   :TREE-MENU
	   :SEQUENCING-MENU
	   
	   :SEND-UDP
	   :OSC-LISTEN

	   :EUCLIDEAN
	   :EXP-DECAY
	   :FN-MEX
	   :GAUSS
	   :QUADRARE
	   :RND-MAP

	   :LAMBDA*
	   :SCALING
	   :MAPPING

           :DIFFERENTIAL-VECTOR

	   :*ALL-SEQUENCING*
	   :CREATE-SEQUENCING
	   :SET-CORPUS
           :SET-PULSE
           :SET-RULE
           :SET-ROUTINE
           :SET-SUBROUTINE
           :ACT-ROUTINE
	   :KILL-ROUTINE
	   ))

(progn
  (ignore-errors (require 'fosc))
  (unless (find-package 'fosc) (ignore-errors (require 'osc))))

(in-package :N3)
(setf *random-state* (make-random-state t)
      *print-pretty* nil)

;------------------------------------------------------------------
;; src: https://stackoverflow.com/a/60816019/3224092
#+sbcl
(defclass fn-with-code (sb-mop:funcallable-standard-object)
  ((code :reader source-of :initarg :source)
   (function :reader function-of :initarg :function)
   (ml :reader save-as :initarg :ml))
  (:metaclass sb-mop:funcallable-standard-class))
#+openmcl
(defclass fn-with-code (ccl:funcallable-standard-object)
  ((code :reader source-of :initarg :source)
   (function :reader function-of :initarg :function)
   (ml :reader save-as :initarg :ml))
  (:metaclass ccl:funcallable-standard-class))

(defmethod initialize-instance :after ((f fn-with-code) &key &allow-other-keys)
  #+sbcl
  (sb-mop:set-funcallable-instance-function f (function-of f))
  #+openmcl
  (ccl:set-funcallable-instance-function f (function-of f)))       

(defun make-fn-with-code (function source ml)
  (make-instance 'fn-with-code :source source :function function :ml ml))

(defmacro lambda* ((&rest args) &body body)
  (let ((code `(lambda ,args ,@body))
	(ml `(lambda* ,args ,@body)))
    `(make-fn-with-code ,code ',code ',ml)))

(defgeneric ml! (o)
  (:method ((o fn-with-code)) (save-as o))
  (:method ((o t)) (declare (ignore o)) nil))
(defgeneric ml? (o)
  (:method ((o fn-with-code)) (declare (ignore o)) t)
  (:method ((o t)) (declare (ignore o)) nil))

(defmethod print-object ((o fn-with-code) stream)
  (print-unreadable-object (o stream :type nil :identity nil)
    (format stream "FUNCTION ~S" (source-of o))))

; > (lambda* (x y) (* x y))
; #<FUNCTION (LAMBDA (X Y) (* X Y))>   
;------------------------------------------------------------------
