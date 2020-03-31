(progn
  (ignore-errors (require 'fosc))
  (unless (find-package 'fosc) (ignore-errors (require 'osc))))

(defpackage :N3
  (:use :cl)
  (:export :*N3-BACKUP-DIRECTORY* 
	   :*AVAILABLE-SOM*
	   :*AVAILABLE-AREA*
	   :CREATE-MLT
	   :CREATE-AREA
	   :LOCATE-CLIQUE
	   :LOCATE-TOURNOI
	   :LOCATE-CYCLE
	   :UPDATE-FANAUX
	   :UPDATE-COVER-VALUE
	   :LEARN
	   :SAVE
	   :LOAD-NEURAL-NETWORK
	   :NEXT-EVENT-PROBABILITY
	  
	   :*TREE*
	   :GET-LEAVES
	   :CAH-FANAUX
	   :DENDROGRAM

	   :NET-MENU
	   :TREE-MENU
	   
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
	   :MAPPING))

(in-package :N3)
(setf *random-state* (make-random-state t))

;------------------------------------------------------------------
(ignore-errors (require 'closer-mop))
(cond ((find-package 'closer-mop)

       (format t "loaded: CLOSER-MOP~%")
       
       ;; src: https://stackoverflow.com/a/60816019/3224092
       (defclass fn-with-code (c2mop:funcallable-standard-object)
	 ((code :reader source-of :initarg :source)
	  (function :reader function-of :initarg :function)
	  (ml :reader save-as :initarg :ml))
	 (:metaclass c2mop:funcallable-standard-class))
       
       (defmethod initialize-instance :after ((f fn-with-code) &key &allow-other-keys)
		  (c2mop:set-funcallable-instance-function f (function-of f)))
       
       (defun make-fn-with-code (function source ml)
	 (make-instance 'fn-with-code :source source :function function :ml ml))
       
       (defmacro lambda* ((&rest args) &body body)
	 (let ((code `(lambda ,args ,@body))
	       (ml `(lambda* ,args ,@body)))
	   `(make-fn-with-code ,code ',code ',ml)))

       (defgeneric ml! (o) (:method ((o fn-with-code)) (save-as o)))
       (defgeneric ml? (o)
	 (:method ((o fn-with-code)) (declare (ignore o)) t)
	 (:method ((o t)) (declare (ignore o)) nil))
       
       (defmethod print-object ((o fn-with-code) stream)
	 (print-unreadable-object (o stream :type nil :identity nil)
	   (format stream "FUNCTION ~S" (source-of o)))))
      
      ; > (lambda* (x y) (* x y))
      ; #<FUNCTION (LAMBDA (X Y) (* X Y))> 

      (t
       (warn "The package CLOSER-MOP is recommanded to keep track of the 'source code' of lambda functions (see MACRO lambda*).")
       (defmacro lambda* ((&rest args) &body body) `(lambda ,args ,@body))))   
;------------------------------------------------------------------
