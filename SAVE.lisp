;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                              SAVE

(defgeneric save (self))

(defvar *UPDATE-SAVED-NET* (concatenate 'string *NEUROMUSE3-DIRECTORY* "bin/update-saved-net"))

(defun get-slots (object)
  #+sbcl(mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+openmcl(mapcar #'ccl:slot-definition-name (ccl:class-slots (class-of object))))

(defmethod save ((self som))
  (let ((path (format nil "~A~A.som" *N3-BACKUP-DIRECTORY* (name self))))
    (let ((slots-som (get-slots self))
	  (slots-neuron (get-slots (id (car (neurons-list self))))))
      (with-open-file (stream path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(if (mlt-p self)
	    (format stream "(PUSH (MAKE-INSTANCE (QUOTE N3::MLT)")
	    (format stream "(PUSH (MAKE-INSTANCE (QUOTE N3::SOM)"))
	(loop for s in slots-som do
	     (let ((val (funcall s self)))
	       (if (eq s (read-from-string "NEURONS-LIST")) 
		   (progn (format stream " :NEURONS-LIST (LIST")
			  (loop for nl in (neurons-list self) do
			       (format stream " (MAKE-INSTANCE (QUOTE N3::NEURON)")
			       (loop for n in slots-neuron do
				 (if (eq n (read-from-string "NAME"))
				     (format stream " :NAME (READ-FROM-STRING (STRING (GENSYM (FORMAT NIL \"NEURON-\"))))")
				     (let ((val-n (funcall n nl)))
				       (format stream " :~S (QUOTE ~S)" n val-n))))	       
			       (format stream ")"))
			  (format stream ")"))
		   (cond ((hash-table-p val) (format stream " :~S (MAKE-HASH-TABLE :TEST #'EQUALP)" s))
			 ((functionp val) 
			  (let ((mvl (multiple-value-list (function-lambda-expression val))))
			    (cond
			      ((listp (carlast mvl)) (format stream " :~S ~S" s (if (ml? val) (ml! val) val)))				
			      (t (format stream " :~S #'~S" s (carlast mvl))))))
			 ((eq s (read-from-string "FANAUX-LIST")) (format stream " :FANAUX-LIST NIL"))
			 ((eq s (read-from-string "GHOST")) (format stream " :GHOST (MAKE-INSTANCE (QUOTE N3::NEURON) :NAME (QUOTE GHOST) :NET (QUOTE ~S))" self))
			 ((eq s (read-from-string "NEURON-GAGNANT")) (format stream " :NEURON-GAGNANT NIL"))
			 (t (format stream " :~S (QUOTE ~S)" s val))))))
	(format stream ") N3::*ALL-SOM*) ")
	(format stream "(DEFVAR ~S (SYMBOL-VALUE ~S)) " (name self) self)
	(when (mlt-p self)
	  (maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (ONSET ~S)) ~S) " k (name self) v)) (onset self))
	  (maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (FINE ~S)) ~S) " k (name self) v)) (fine self))
	  (maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (TRNS ~S)) ~S) " k (name self) v)) (trns self))
	  (maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (ARCS ~S)) ~S) " k (name self) v)) (arcs self)))
	(maphash (lambda (k v)
		   (if (ds-p v)
		       (if (and (eq :bypass (car (list! (dt v)))) (ml? (cadr (list! (dt v)))))
			   (format stream "(SETF (GETHASH (QUOTE ~S) (DATE-REPORT ~S)) (MAKE-INSTANCE 'DS :dt (LIST :BYPASS ~S))) " k (name self) (ml! (cadr (dt v))))
			   (format stream "(SETF (GETHASH (QUOTE ~S) (DATE-REPORT ~S)) (MAKE-INSTANCE 'DS :dt (QUOTE ~S))) " k (name self) (list! (dt v))))
		       (format stream "(SETF (GETHASH (QUOTE ~S) (DATE-REPORT ~S)) ~S) " k (name self) v)))
		 (date-report self))
	(when (mlt-p self) (format stream "(SETF (FANAUX-LIST ~S) (LOOP FOR I IN '~S COLLECT (NTH I (NEURONS-LIST ~S)))) " self (loop for i in (fanaux-list self) collect (ind (id i))) self))
	(format stream "(SETF (NEURON-GAGNANT ~S) (WINNER ~S))" self self)))
    (UIOP:run-program (format nil "sh -c '~S ~S'" *UPDATE-SAVED-NET* path))))

(defmethod save ((self area))
  (let ((path (format nil "~A~A.area" *N3-BACKUP-DIRECTORY* (name self))))
    (loop for i in (soms-list self) do (save (id i)))
    (let ((slots-som (get-slots self)))
      (with-open-file (stream path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream "(PUSH (MAKE-INSTANCE (QUOTE N3::AREA)")
	(loop for s in slots-som do
	     (let ((val (funcall s self)))
	       (if (hash-table-p val)
		   (format stream " :~S (MAKE-HASH-TABLE :TEST #'EQUALP)" s)	   
		   (format stream " :~S (QUOTE ~S)" s val))))
	(format stream ") N3::*ALL-AREA*)")
	(format stream "(DEFVAR ~S (SYMBOL-VALUE ~S))" (name self) self)
	(maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (ARCS ~S)) ~S) " k self v)) (arcs self))
	(maphash (lambda (k v) (format stream "(SETF (GETHASH (QUOTE ~S) (DATE-REPORT ~S)) ~S) " k (name self) v)) (date-report self))))
    (UIOP:run-program (format nil "sh -c '~S ~S'" *UPDATE-SAVED-NET* path))))

;------------------------------------------------------------------
;                                               LOAD-NEURAL-NETWORK

(defun all-pos (item lst)
  (loop for i in lst and position from 0
        when (equalp item i)
        collect position))

(defun load-neural-network (nn &optional kw) ;; kw -> :only-area :copy :copy-only-area :ignore-warning
  "nn is a string meaning neural network.
   Just write full pathname of nn
   [for instance /User/.../FOO.area as string];
   or if the path of the instance is in *N3-BACKUP-DIRECTORY*,
   just write the name of the instance."
  (let ((file (if (zerop (length (directory-namestring (pathname nn))))
		  (let ((tmpfile (format nil "~A~A.som" *N3-BACKUP-DIRECTORY* nn)))
		    (if (open tmpfile :if-does-not-exist nil)
			tmpfile
			(format nil "~A~A.area" *N3-BACKUP-DIRECTORY* nn)))
		  nn))
	(sta (if (or (eq kw :copy) (eq kw :copy-only-area)) "copied" "loaded"))) 
    (if (open file :if-does-not-exist nil)
	(let ((tn (pathname-type (pathname file)))
	      (nn (pathname-name (pathname file)))) 
	  ;;[TODO] warning if net is not loaded when SOM/MLT
	  (cond ((equalp tn "som") (if (member (id (read-from-string nn)) *ALL-SOM*)
				       (when (eq kw :ignore-warning) (warn "There is already a SOM called ~A in *ALL-SOM*. Consequently, this SOM has not been loaded." nn))
				       (progn (load file)
					      (format t "~45<~A.~(~a~) ...~;... ~A ...~>~%" nn tn sta))))
		((equalp tn "area") (if (or (eq kw :only-area) (eq kw :copy-only-area))
					(if (member (id (read-from-string nn)) *ALL-AREA*)
					    (when (eq kw :ignore-warning) (warn "There is already an AREA called ~A in *ALL-AREA*. Consequently, this AREA has not been loaded." nn))
					    (progn (load file)
						   (format t "~45<~A.~(~a~) ...~;... ~A ...~>~%" nn tn sta)))
					(let* ((sl (let* ((in (open file))
							  (out (format nil "~a~%" (read-line in)))) 
						      #+openmcl (read-from-string (remove #\' (format nil "~S" (nth 5 (cadr (read-from-string out))))))
						      #+sbcl (eval (read-from-string (remove #\' (format nil "~S" (nth 5 (cadr (read-from-string out)))))))
						     ))
					       (dir (directory-namestring (pathname file)))
					       (il (let* ((in (open file))
							  (out (format nil "~a~%" (read-line in)))) 
						      #+openmcl (read-from-string (remove #\' (format nil "~S" (nth 7 (cadr (read-from-string out))))))
						      #+sbcl (eval (read-from-string (remove #\' (format nil "~S" (nth 7 (cadr (read-from-string out)))))))
						     ))
					       (lstest (all-pos nil (loop for i in sl collect (if (open (format nil "~A~S.som" dir i) :if-does-not-exist nil) t nil)))))
					  (if (null lstest)
					      (progn
						(loop for s in sl do (load-neural-network (format nil "~A~S.som" dir s)))
						(if (equalp (loop for l in il collect (if (null l) 0 l)) (loop for s in sl collect (length (fanaux-list (id s)))))
						    (if (member (id (read-from-string nn)) *ALL-AREA*)
							(when (eq kw :ignore-warning) (warn "There is already an AREA called ~A in *ALL-AREA*. Consequently, this AREA has not been loaded." nn))
							(progn (load file)
							       (format t "~45<~A.~(~a~) ...~;... ~A ...~>~%" nn tn sta)))
						    (when (eq kw :ignore-warning) (warn "There is no agreement between the fanaux-list of soms-list and the fanaux-length. This AREA can't be loaded."))))
					      (when (eq kw :ignore-warning) (warn "The file~A~{ ~A.som~} do~A not exist [at least in \"~A\"]. Consequently, this AREA can't be loaded." (if (= 1 (length lstest)) "" "s") (loop for i in lstest collect (nth i sl)) (if (= 1 (length lstest)) "es" "") (read-from-string dir))))))) 
		(t (when (eq kw :ignore-warning) (warn "This file is not identified as part of N3.")))))
	(when (eq kw :ignore-warning) (warn "This file does not exist.")))))

;------------------------------------------------------------------
