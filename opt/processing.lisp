;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                        PROCESSING    

(defgeneric init-processing (self folder)
  (:method ((self area) (folder t))
    (let ((dir (concatenate 'string (expand-path "~/Documents/Processing/") (format nil "~A/" folder))))
      (if (probe-file dir)
	  (>data-file (ensure-directories-exist (concatenate 'string dir (format nil "data/~A.txt"  self))) 
		      (flat-once (loop for s in (soms-list self) for i from 0
				       collect
				       (loop for f in (fanaux-list (id s)) for j from 0
					     collect
					     (cons i (cons j (xpos (id f)))))))
		      :out :csv)
	  (warn "~A does not exists!" dir)))))

;; writing data in ~/Documents/Processing/<folder>/
;; (init-processing corpus 'N3_P5)
;------------------------------------------------------------------
