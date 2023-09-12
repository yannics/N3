;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                              COPY

(defvar *COPY-FILE* (concatenate 'string *NEUROMUSE3-DIRECTORY* "bin/copy-file"))

(defgeneric reset-mlt (self)
  (:method ((self mlt)) 
    (setf (epoch self) 0
	  (mct self) nil
	  (onset self) (make-hash-table :test #'equalp)
	  (fine self) (make-hash-table :test #'equalp)
	  (trns self) (make-hash-table :test #'equalp)
	  (arcs self) (make-hash-table :test #'equalp))))

(defgeneric reset-area (self)
  (:method ((self area)) 
    (setf (current-clique self) nil
	  (arcs self) (make-hash-table :test #'equalp))
    (loop for i in (soms-list self) do (reset-mlt (id i)))))

(defun is-in-n3-backup-directory? (name ext &optional (dir *N3-BACKUP-DIRECTORY*))
  (let ((std (string-to-list (string-trim '(#\Space #\Tab #\Newline) (with-output-to-string (out) (UIOP:run-program (format nil "sh -c 'basename -as .~A `ls ~A*.~A`'" ext dir ext) :output out))))))
    (member (read-from-string (string name)) std)))

(defgeneric copy (self &key newname reset with))

(defmethod copy ((self som) &key newname reset with)
  (declare (ignore with))
  (if (and newname
	   (or (boundp (read-from-string (string newname)))
	       (is-in-n3-backup-directory? newname "som")))
      (warn "... ~A already bounds/exists! Please try another new name ..." newname)
      (progn
	(unless newname (let ((nn (make-new-symbol (name self)))) (setf newname (read-from-string (string nn))) (makunbound nn)))
	(UIOP:run-program (format nil "sh -c '~S ~S ~S'" *COPY-FILE* (concatenate 'string *N3-BACKUP-DIRECTORY* (string (name self)) ".som") newname))
	(load-neural-network (string newname) :copy)
	(let ((date (get-universal-time))
	      (init (replace-all (replace-all (gethash (apply #'min (ht (date-report self) :k)) (date-report self)) "init-som" (format nil "copied-som [~A]" self)) (format nil "#<RNA ~A>" self) (format nil "#<RNA ~A>" newname)))
	      (ds (the-ds self)))
	  (setf (date-report (car *all-som*)) (make-hash-table)
		(gethash date (date-report (car *all-som*))) init)
	  (when ds (setf (gethash (1+ date) (date-report (car *all-som*))) ds)))
	(when reset (reset-mlt (car *all-som*)))
	(save (car *all-som*)))))

(defmethod copy ((self area) &key newname reset with)
  (if (and newname
	   (or (boundp (read-from-string (string newname)))
	       (is-in-n3-backup-directory? newname "area")))
      (warn "... ~A already bounds/exists! Please try another new name ..." newname)
      (let ((ok
	      (cond ((and (listp with)
			  (= (length with) (length (soms-list self)))
			  (loop for i in with never (or (boundp (read-from-string (string i))) (is-in-n3-backup-directory? i "area"))))
		     (loop for i in with for j in (soms-list self) do (copy (id j) :newname i)) with)
		    ((null with) (loop for i in (soms-list self) do (copy (id i))) (reverse (subseq *all-som* 0 3)))
		    (t (warn "... the soms-list proposed has either a SOM already bounded or existed, or either unexpected list of objects! Please try another soms-list ...")))))
	(when ok
	  (progn
	    (unless newname (let ((nn (make-new-symbol (name self)))) (setf newname (read-from-string (string nn))) (makunbound nn)))
	    (UIOP:run-program (format nil "sh -c '~S ~S ~S'" *COPY-FILE* (concatenate 'string *N3-BACKUP-DIRECTORY* (string (name self)) ".area") newname))
	    (load-neural-network (string newname) :copy-only-area)
	    (setf (soms-list (car *all-area*)) ok)
	    ;(format t "AREA copied with :SOMS-LIST ~S" ok)
	    (when reset (reset-area (car *all-area*)))
	    (let ((date (get-universal-time))
		  (init (replace-all (replace-all (replace-all (gethash (apply #'min (ht (date-report self) :k)) (date-report self)) "init-area" (format nil "copied-area [~A]" self)) (format nil "#<AREA ~A>" self) (format nil "#<AREA ~A>" newname)) (format nil "#<SL ~S>" (soms-list self)) (format nil "#<SL ~S>" ok))))
	      (setf (date-report (car *all-area*)) (make-hash-table)
		    (gethash date (date-report (car *all-area*))) init))
	    (save (car *all-area*)))))))

(defmethod copy ((self sequencing) &key newname reset with)
  (declare (ignore reset with))
  (unless newname (setf newname (make-new-symbol (dub self))))
  (if (or (boundp (read-from-string (string newname)))
	  (is-in-n3-backup-directory? newname "seq"))
      (warn "... ~A already bounds/exists! Please try another new name ..." newname)
      (progn
	(UIOP:run-program (format nil "sh -c '~S ~S ~S'" *COPY-FILE* (concatenate 'string *N3-BACKUP-DIRECTORY* (string (name self)) ".seq") newname))
	(scan-seq (concatenate 'string *N3-BACKUP-DIRECTORY* (string newname) ".seq") nil t)
	(setf (description (car *all-sequencing*)) (concatenate 'string (format nil "[copy of ~A[~A]] " (name (car *all-sequencing*)) (dub (car *all-sequencing*))) (description (car *all-sequencing*))))
	(save (car *all-sequencing*)))))

;------------------------------------------------------------------
