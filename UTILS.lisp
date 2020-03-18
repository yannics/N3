;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                    READ-DATA-FILE
 
(defun read-text-lines (file)
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop with length = (file-length in-stream)
       while (< (file-position in-stream) length)
       collect (read-line in-stream))))

(defun string-to-list (string)
  (let ((the-list nil) 
        (end-marker (gensym)))
    (loop (multiple-value-bind (returned-value end-position)
	      (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            (setq the-list 
                  (append the-list (list returned-value)))
            (setq string (subseq string end-position))))))

(defun read-file (file)
  (loop for i in (read-text-lines (expand-path (if (stringp file) file (namestring file)))) collect (string-to-list (string-trim '(#\Space #\Tab #\Newline) i))))

;------------------------------------------------------------------
;                                                             UTILS

(defun ht (ht &aux (*print-pretty* t))
  (format t "~&~s~%" ht)
  (maphash (lambda (k v) (format t "~@<~S~20T~3I~_~S~:>~%" k v)) ht)
  (values))

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

;; convert the hash table into an association list
;; from the Alexandria library
(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun sort-hash-table-by-keys (table)
  "Keys are expected to be numeric."
  (ordinate (hash-table-alist table) #'> :key #'car))

(defvar *days* '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defgeneric history (self &optional opt))
(defmethod history ((self t) &optional opt)
  (declare (ignore opt))
  (when (or (som-p (id self)) (area-p (id self)))
    (labels ((tps (gdt)
	       (multiple-value-bind
		     (second minute hour date month year day-of-week dst-p tz)
		   (decode-universal-time gdt)
		 (declare (ignore dst-p))
		 (format nil "#<~a ~2,'0d:~2,'0d:~2,'0d ~2,'0d/~2,'0d/~d (GMT~@d)>" (nth day-of-week *days*) hour minute second date month year (- tz)))))
      (let ((mht (sort-hash-table-by-keys (date-report self))))
	(loop for i in (butlast mht) do (format t "Modified: ~a~&" (tps (car i))) (format t (cdr i)) (format t "~&"))
	(format t "Created: ~a~&---> ~a~&" (tps (caar (last mht))) (cdar (last mht)))))))

;;; ;;  ;;;;   ;; ;; ;  ;   ;   ; 
	    
(defun split-symbol (symbol &optional (delim-char #\.))
  (let ((s (symbol-name symbol)))
    (loop for i = 0 then (1+ j)
       as j = (position delim-char s :start i)
       collect (read-from-string (subseq s i j))
       while j)))

(defun net-menu ()
  (let* ((stdout (format nil "~a" (UIOP:run-program (format nil "sh -c 'cd ~S; for f in *.som; do echo $f; done; for f in *.area; do echo $f; done'" *N3-BACKUP-DIRECTORY*) :output :string)))
	 (items (string-to-list stdout)))
    (when items
      (format t ";---------------")
      (dotimes (item-number (length items))
	(format t "~%~A: ~A" (+ item-number 1) (nth item-number items)))
      (format t "~%Load (Type any key to escape): ")
      (let ((val (read)))
	(when (and (integerp val) (<= val (length items)) (> val 0)) (load-neural-network (string (car (split-symbol (nth (- val 1) items))))))))))

;;;; ;; ;;  ; ;;;  ;; ;  ; ; ; ;   ;

(defun >data-file (path lst &optional comment)
  "Allows to write data file. 
If needed it can be added a comment in first line started with:
';' = LISP
'#' = GNUPLOT
..."
  (with-open-file (stream (make-pathname :directory (pathname-directory path)
					   :name (pathname-name path)
					   :type (pathname-type path))
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (when comment (format stream "~A~&" comment))
    (loop for i in lst
       do
	 (format stream "~{~S ~}~%" (if (listp i) i (list i))))))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defgeneric >dot (self nodes)
  (:documentation "Allow to draw a graph dot to visualize all connections in the net <self> following <nodes> list.
When self is MLT, nodes has to be formed for instance as follow:
nodes = ((2 ? ? 1) nil); that means (car nodes) = tournoi with wild cards for every unknown involving (length (car nodes)) = order; and (cadr nodes) = remanence (t or nil)"))

(defmethod >dot ((self area) (nodes-lst list))
  (let* ((el (remove-duplicates (loop for key being the hash-keys of (arcs self) collect key) :test #'(lambda (a b) (equalp a (reverse b)))))
	 (ed (loop for i in nodes-lst collect (loop for j in el when (or (equalp i (car j)) (equalp i (cadr j))) collect j)))
	 (cil (count-item-in-list (flat-once (flat-once ed))))
	 (ped (loop for i in cil when (and (not (member (cadr i) nodes-lst :test #'equalp)) (>= (car i) (length nodes-lst))) collect (cadr i)))
	 (out (format nil "~A~S.dot" *N3-BACKUP-DIRECTORY* self))
	 (scriptpath (format nil "~Abin/dot2img" *NEUROMUSE3-DIRECTORY*)))
    (with-open-file (out (make-pathname :directory (pathname-directory out)
					:name (pathname-name out)
					:type (pathname-type out))
			 :direction :output 
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "graph ~S {~&" (pathname-name out))
      (format out "overlap=false;~&")
      (format out "splines=true;~&")
      (format out "{node [style=filled,color=skyblue] ~{ \"~S\"~}}~&" nodes-lst)
      (loop for i in el
	 do
	   (format out "\"~S\" -- \"~S\" [style=~(~A~)];~&" 
		   (car i) (cadr i) 
		   (if (null nodes-lst) 
		       'filled 
		       (if (or (member (car i) nodes-lst :test #'equalp) (member (cadr i) nodes-lst :test #'equalp))
			   (if (or (member (car i) ped :test #'equalp) 
				   (member (cadr i) ped :test #'equalp)
				   (and (member (car i) nodes-lst :test #'equalp)
					(member (cadr i) nodes-lst :test #'equalp)))
			       'filled
			       'dotted)
			   'invis))))
      (format out "}"))
    (UIOP:run-program (format nil "sh -c '~S ~S neato ~S'" scriptpath out *display*))))

(defmethod >dot ((self mlt) (nodes-lst list))
  (let* ((el (loop for key being the hash-keys of (arcs self) collect key))
	 (nl (loop for i in (car nodes-lst) when (numberp i) collect i)) 
	 (ed (flat-once (loop for i in nl collect (loop for j in el when (or (equalp i (car j)) (equalp i (cadr j))) collect j))))
	 (ped (remove-duplicates (flat-once (mapcar #'get-arc-from-tournoi (locate-tournoi self (car nodes-lst) :remanence (cadr nodes-lst)))) :test #'equalp))
	 (out (format nil "~A~S.dot" *N3-BACKUP-DIRECTORY* self))
	 (scriptpath (format nil "~Abin/dot2img" *NEUROMUSE3-DIRECTORY*)))
    (with-open-file (out (make-pathname :directory (pathname-directory out)
					:name (pathname-name out)
					:type (pathname-type out))
			 :direction :output 
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "digraph ~S {~&" (pathname-name out))
      (format out "concentrate=true")
      (format out "{node [style=filled,color=skyblue] ~{ \"~S\"~}}~&" nl)
      (format out "edge [arrowsize=.7];")
      (loop for i in el
	 do
	   (format out "\"~S\" -> \"~S\" [style=~(~A~)];~&" 
		   (car i) (cadr i) 
		   (if (null (car nodes-lst)) 
		       'filled 
		       (if (member i ed :test #'equalp)
			   (if (or (member i ped :test #'equalp) 
				   (member i nodes-lst :test #'equalp))
			       'filled
			       'dotted)
			   'invis))))
      (format out "}"))
    (UIOP:run-program (format nil "sh -c '~S ~S dot ~S'" scriptpath out *display*))))

;------------------------------------------------------------------

(defun flat-1 (lst) (if (consp (car lst)) (apply 'append lst) lst))

(defun combx (vals n)
  "From PW: Creates all combinations of n elements from vals."
   ; ATTENTION: Not always equal to (+ n 1) per a problem in recursion ...???
  (cond
    ((<=  n 0) vals)
    (t (flat-1
	(mapcar #'(lambda (x) (mapcar #'(lambda (y) (append (list! x) (list! y))) (list! vals))) (list! (combx vals (1- n))))))))

(defun all-combinations (vals n)
  "From old PatchWork: it creates all combinations of the given list in vals with a length set in n."
  (let ((n (1- n)))
    (combx vals n)))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defgeneric all-tournoi (self &key order remanence))
(defgeneric all-clique (self))
(defgeneric rnd-tournoi (self &key order remanence))
(defgeneric rnd-clique (self))

(defun ser-list (n)
  (loop for ser from 0 to (1- n) collect ser))

(defmethod all-tournoi ((self mlt) &key (order (cover-value self)) (remanence t))
  (when (and (integerp order) (> order 1))
    (if remanence
	(let ((res (if (= order (cover-value self))
		       (loop for he in (hash-table-alist (trns self)) collect (list (cdr he) (car he)))
		       (let* ((allht (loop for value being the hash-values of (trns self) using (hash-key key) collect (list value key)))
			      (allwo (if (> order (cover-value self))
					 (remove-duplicates (loop for i in allht append (locate-tournoi self (complist (cadr i) order '?))) :test #'equalp :key #'cadr)
					 (loop for k in allht append (loop for j in (step-wind (cadr k) order) collect (list (car k) j)))))
			      (htmp (make-hash-table :test #'equalp)))
			 (loop for i in allwo
			    do
			      (setf (gethash (cadr i) htmp)
				    (if (gethash (cadr i) htmp)
					(+ (gethash (cadr i) htmp) (car i))
					(car i))))
			 (loop for he in (hash-table-alist htmp) collect (list (cdr he) (car he)))))))
	  (ordinate (mapcar #'list (mapcar #'float (normalize-sum (mapcar #'car res))) (mapcar #'cadr res)) #'> :key #'car))      
	(let ((res (loop for i in (all-combinations (ser-list (length (fanaux-list self))) order) when (tournoi-p i self :arcs) collect i)))
	  (ordinate (mapcar #'list (mapcar #'float (normalize-sum (get-weight self res :remanence nil))) res) #'> :key #'car)))))

(defmethod rnd-tournoi ((self mlt) &key (order (cover-value self)) (remanence t))
  (rnd-weighted (mapcar #'reverse (all-tournoi self :remanence remanence :order order))))

(defmethod all-clique ((self area))
  (let ((tmp (loop for i in (reccomb (ser-list (car (fanaux-length self))) (mapcar #'ser-list (cdr (fanaux-length self)))) when (clique-p i self) collect i)))
    (ordinate (mapcar #'list (mapcar #'float (normalize-sum (get-weight self tmp))) tmp) #'> :key #'car)))

(defmethod rnd-clique ((self area))
  (rnd-weighted (mapcar #'reverse (all-clique self))))

;------------------------------------------------------------------

