;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                              OPND

#|
---> http://www.titanmusic.com/data.php
... OPNDV format
Each file contains a Lisp list of elements.
Each element represents either a note in the score or a sequence of tied notes.
Each element is itself a list, with the format (o p d v) where o is the onset time in tatums and p is the pitch name in standard ASA format but with "n" for natural, "s" for sharp and "f" for flat (s and f can be repeated any number of times to give multiple sharps or flats). d is the duration in tatums and v is an integer indicating the voice to which the note belongs.
...
|#

(defparameter *notename* '("C" "" "D" "" "E" "F" "" "G" "" "A" "" "B"))

(defun asa2midi (asa)
  (let* ((lst (loop for i in (coerce (string asa) 'list) collect (if (digit-char-p i) (digit-char-p i) (string i))))
	 (al (remove "N" (butlast (cdr lst)) :test #'equalp))
	 (fact (cond ((equalp "S" (car al)) (* (length al) 1))
		     ((equalp "F" (car al)) (* (length al) -1))
		     (t 0))))
    (+ fact (position (car lst) *notename* :test #'string=) (* (1+ (car (last lst))) 12))))

(defun >int (a b) (when (and a b) (remove-duplicates (flatten (loop for i in a collect (loop for j in b collect (- j i)))))))

(defun mod12 (x) (mod x 12))

(defun map-opnd (file)
  (let* ((opnd (caar (read-file file)))
	 (voice (reduce #'max (mapcar #'cadddr opnd)))
	 (lst (loop for nv from 1 to voice collect (loop for i in opnd when (= (car (last i)) nv) collect i))))

    (loop for score in lst collect 
	 (let ((r (list (list (caar score) (caddar score) (list (asa2midi (cadar score)))))))
	   (loop for a in (cdr score) do
		(if (eq (car a) (caar r))
		    (setf r (cons (list (car a) (apply #'min (caddr a) (caddar r)) (cons (asa2midi (cadr a)) (caddar r))) (cdr r)))
		    (push (list (car a) (caddr a) (list! (asa2midi (cadr a)))) r)))

	   ;; for now I will consider only the first item ...
	   ;; (loop for ev in (reverse r) for i from 1 collect (list (cadr ev) (mapcar #'mod12 (caddr ev)) (>int (caddr ev) (caddr (nth i (reverse r))))))
	   (loop for ev in (reverse r) for i from 1 collect (list (cadr ev) (car (mapcar #'mod12 (caddr ev))) (car (>int (caddr ev) (caddr (nth i (reverse r))))))))))) 
	   
;; (defparameter +corpus+ (flat-once (mapcar #'map-opnd (directory "~/opnd-files/*b-mel.opnd"))))

;------------------------------------------------------------------


