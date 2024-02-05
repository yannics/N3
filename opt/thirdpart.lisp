;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                    INITIALISATION    

(defgeneric init-process (self dest)
  ;; TODO as keys:
  ;; :folder as data by default or nil for no data folder or to name this folder
  ;; :out as csv by default or a given delimiter (can be nil as a space delimiter)
  ;; (self list) as a given list to initiate process.
  (:method ((self area) (dest string))
    (if (probe-file dest)
	(>data-file (ensure-directories-exist (concatenate 'string dest (format nil "data/~A.txt" self))) 
		    (flat-once (loop for s in (soms-list self) for i from 0
				     collect
				     (loop for f in (fanaux-list (id s)) for j from 0
					   collect
					   (cons i (cons j (xpos (id f)))))))
		    :out :csv)
	(warn "~A does not exists!" dest))))

;; (init-process corpus "~/Documents/Processing/N3_P5")
;------------------------------------------------------------------
;                                                       OSC MESSAGE

(defvar *thirdpart* nil)

(defun thirdpart-message (fn) ;; fn = lambda* function
  (push fn (gethash 'thirdpart (mem-cache *orgue*))))

;; https://www.cadtutor.net/forum/topic/37995-remove-element-from-list/?do=findComment&comment=310356
(defun remnth (n lst)
  (if (> n 0)
      (cons (car lst) (remnth (1- n) (cdr lst)))
      (cdr lst)))

(defun remove-thirdpart (nth sequencing)
  (setf (gethash 'thirdpart (mem-cache (id sequencing)))
	(remnth nth (gethash 'thirdpart (mem-cache (id sequencing))))))
  
;------------------------------------------------------------------
