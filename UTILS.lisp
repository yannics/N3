;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)
(defvar *debug* nil)

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

(defun version () (format t "~a" (asdf:component-version (asdf:find-system "n3"))))

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

(defgeneric the-ds (self) (:method ((self mlt)) (cdar (ordinate (loop for i in (ht (date-report self) :al) when (ds-p (cdr i)) collect i) '> :key #'car))))
(defun addtothird (n lst) (cond ((= 2 (length lst)) (reverse (cons (if n n 0) (reverse lst)))) ((= 3 (length lst)) (reverse (cons (if n n 0) (reverse (butlast lst))))) (t nil)))

(defgeneric update-data-scale (self &rest ds)
  (:documentation "DS [as key + data-scale value] {default-value}
+-----------------------------------------------------------------------+
|    :norm (min max curve{0}) or curve                                  |
| OR :dim ((min[1] max[1] curve[1]{0}) ... (min[n] max[n] curve[n]{0})) |               
+-----------------------------------------------------------------------+
| OR :bypass [as identity set by default]                               |
+-----------------------------------------------------------------------+
Note that the output is clipped if the range of input is largest than values of :norm or :dim")
  (:method ((self mlt) &rest ds)
    ;; test ds
    (if (or (and (eq :bypass (car ds)) (or (eq (cadr ds) 't) (null (cadr ds))))
	    (and (eq :norm (car ds)) (listp (cadr ds)) (or (= 2 (length (cadr ds))) (= 3 (length (cadr ds)))) (loop for n in (cadr ds) always (numberp n)) (null (caddr ds)))
	    (and (eq :dim (car ds)) (listp (cadr ds)) (loop for x in (cadr ds) always (and (listp x) (or (= 2 (length x)) (= 3 (length x))) (loop for y in x always (numberp y)))) (null (caddr ds))))
	;; add DS to history of self
	(if (equalp (dt (the-ds self)) ds)
	    (warn "Nothing to update! ...")
	    (let ((ct (get-universal-time)))
	      (setf (gethash ct (date-report self)) (make-instance 'ds :dt ds))
	      (the-ds self)))
    (warn "Nothing to update! Check your DS entry..."))))

(defgeneric zero-a (x &optional quasi-zero)
 (:documentation "Replace all quasi-zero by zero.")
 ;; zero-a is [...] a kind of deleuzian un-differentiation...
 (:method ((x number) &optional (quasi-zero .00001)) (if (< (abs x) quasi-zero) 0.0 x))
 (:method ((x list) &optional (quasi-zero .00001)) (mapcar #'(lambda (a) (zero-a a quasi-zero)) x)))

(defgeneric scaling (data &key minin maxin minout maxout curve mlt norm dim bypass update))
(defmethod scaling ((data number) &key minin maxin minout maxout curve mlt norm dim bypass update)
  (declare (ignore mlt norm dim bypass update))
  (if (and minin maxin minout maxout curve)
      (if (> (abs curve) 0.001)
	  (let* ((grow (exp curve))
		 (a (/ (- maxin minin) (- 1 grow)))
		 (b (+ minin a)))
	    (zero-a (+ minout (/ (* (log (/ (- b (cond ((> data maxin) maxin) ((< data minin) minin) (t data))) a)) (- maxout minout)) curve))))
	  (zero-a (+ minout (/ (* (- (cond ((> data maxin) maxin) ((< data minin) minin) (t data)) minin) (- maxout minout)) (- maxin minin)))))
      data))

(defmethod scaling ((data list) &key minin maxin minout maxout curve mlt norm dim bypass update)
  (if (mlt-p (id mlt))
      ;;-------------------------------
      (let (ddts)
	(cond (bypass
	       (when *debug* (format t ":bypass"))
	       (setf ddts (list :bypass)))
	      ;;-------------------------------
	      ;; list (minval maximal {curve}) —> if curve unset then 0 
	      ((and (listp norm) (not (null norm)) (or (= 2 (length norm)) (= 3 (length norm))) (loop for n in norm always (numberp n)))
	       (when *debug* (format t ":norm <list & t & [ len=2 | len=3 ] & alwaysNum>"))
	       (setf ddts (list :norm (if (= 2 (length norm)) (addtothird 0 norm) norm))))
	      ;;-------------------------------
	      ;; num —> if ds{norm} then update curve else default vals
	      ((numberp norm)
	       (when *debug* (format t ":norm <num>"))
	       (setf ddts (list :norm (addtothird norm (if (and (eq :norm (car (dt (the-ds (id mlt))))) (cadr (dt (the-ds (id mlt))))) (cadr (dt (the-ds (id mlt)))) (list (reduce #'min (flatten data)) (reduce #'max (flatten data))))))))
	      ;;-------------------------------
	      ;; t —> default vals
	      ((eq norm 't)
	       (when *debug* (format t ":norm <t>"))
	       (setf ddts (list :norm (list (reduce #'min (flatten data)) (reduce #'max (flatten data)) 0))))
	      ;;-------------------------------
	      ;; list ((minval maximal {curve}) and/or curve ...) + ds{dim} —> if list & curve unset then 0; if only curve then update curve with vals of ds
	      ((and (listp dim) (not (null dim)) (= (length (mat-trans data)) (length dim)) (loop for x in dim always (or (and (listp x) (or (= 2 (length x)) (= 3 (length x))) (loop for y in x always (numberp y))) (numberp x))) (eq :dim (car (dt (the-ds (id mlt))))) (cadr (dt (the-ds (id mlt)))))
	       (when *debug* (format t ":dim <list & t & len(mt{data})=len(dim) & always{num | (list & [ len=2 | len=3 ] & alwaysNum)} & ds{dim} & dt{list}>"))
	       (setf ddts (list :dim (loop for i in dim for j in (cadr (dt (the-ds (id mlt)))) collect (if (numberp i) (addtothird i j) (if (= 3 (length i)) i (addtothird 0 i)))))))
	      ;;-------------------------------
	      ;; list ((minval maximal {curve}) and/or curve ...) - ds{dim} —> if list & curve unset then 0; if only curve then update curve with default vals 
	      ((and (listp dim) (not (null dim)) (eq :dim (car (dt (the-ds (id mlt))))) (loop for x in dim always (or (and (listp x) (or (= 2 (length x)) (= 3 (length x))) (loop for y in x always (numberp y))) (numberp x))))
	       (when *debug* (format t ":dim <list & t  & len(mt{data})=len(dim) & always{num | (list & [ len=2 | len=3 ] & alwaysNum)}>"))
	       (setf ddts (list :dim (loop for i in dim for j in (loop for d in (mat-trans data) collect (list (reduce #'min (flatten d)) (reduce #'max (flatten d)) 0)) collect (if (numberp i) (addtothird i j) (if (= 3 (length i)) i (addtothird 0 i)))))))
	      ;;-------------------------------
	      ;; num + ds{dim} —> update all curves with vals of ds
	      ((and (numberp dim) (eq :dim (car (dt (the-ds (id mlt))))) (listp (cadr (dt (the-ds (id mlt))))))
	       (when *debug* (format t ":dim <num & ds{dim} & dt{list}>"))
	       (setf ddts (list :dim (loop for i in (cadr (dt (the-ds (id mlt)))) collect (addtothird dim i)))))
	      ;;-------------------------------
	      ;; num - ds{dim} —> update all curves with default vals 
	      ((numberp dim)
	       (when *debug* (format t ":dim <num>"))
	       (setf ddts (list :dim (loop for i in (loop for d in (mat-trans data) collect (list (reduce #'min (flatten d)) (reduce #'max (flatten d)) 0)) collect (addtothird dim i)))))
	      ;;-------------------------------
	      ;; t —> default vals
	      ((eq dim 't)
	       (when *debug* (format t ":dim <t>"))
	       (setf ddts (list :dim (loop for d in (mat-trans data) collect (list (reduce #'min (flatten d)) (reduce #'max (flatten d)) 0)))))
	      ;;-------------------------------
	      (t
	       (when *debug* (format t ":DS"))
	       (setf ddts (list! (dt (the-ds (id mlt)))))))
	;;-------------------------------
	(when update (apply #'update-data-scale (cons (id mlt) ddts)))
	(case (car ddts)
	  (:bypass data)
	  (:norm (mapcar #'(lambda (x) (scaling x :minin (car (cadr ddts)) :maxin (cadr (cadr ddts)) :minout 0 :maxout 1 :curve (caddr (cadr ddts)))) data))
	  (:dim (mat-trans (loop for d in (mat-trans data) for s in (cadr ddts) collect (scaling d :minin (car s) :maxin (cadr s) :minout 0 :maxout 1 :curve (caddr s)))))))
      ;;-------------------------------
      (mapcar #'(lambda (x) (scaling x :minin (if minin minin (reduce #'min (flatten data))) :maxin (if maxin maxin (reduce #'max (flatten data))) :minout (if minout minout 0) :maxout (if maxout maxout 1) :curve (if curve curve 0))) data)))

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

(defgeneric mapping (self iteration dataset &key init-lr init-rad end-lr end-rad init-ep df ds))
(defmethod mapping ((self som) (iteration integer) (dataset list) &key (init-lr 0.1) (init-rad (/ (funcall #'mean (list! (field self))) 2)) (end-lr 0.01) (end-rad 0.1) (init-ep (epoch self)) (df #'exp-decay) (ds (dt (the-ds self))))
  (let ((data (apply #'scaling (cons dataset (append '(:mlt self) (list! ds) '(t)))))) 
    (dotimes (k iteration)
      (setf (input self)
	    (nth (random (length data)) data)
	    (radius self)
	    (funcall df (1+ (- (epoch self) init-ep)) init-rad iteration end-rad)
	    (learning-rate self)
	    (funcall df (+ 1 (- (epoch self) init-ep)) init-lr iteration end-lr))
      (learn self))))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defvar *days* '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defgeneric history (self &optional opt))
(defmethod history ((self t) &optional opt)
  (declare (ignore opt)) ;; use opt for history recursivity in SL
  (when (or (som-p (id self)) (area-p (id self)))
    (labels ((tps (gdt)
	       (multiple-value-bind
		     (second minute hour date month year day-of-week dst-p tz)
		   (decode-universal-time gdt)
		 (declare (ignore dst-p))
		 (format nil "#<~a ~2,'0d:~2,'0d:~2,'0d ~2,'0d/~2,'0d/~d (GMT~@d)>" (nth day-of-week *days*) hour minute second date month year (- tz)))))
      (let ((mht (ordinate (ht (date-report self) :al) #'> :key #'car)))
	(loop for i in (butlast mht) do
	     (if (ds-p (cdr i))
		 (format t "Updated: ~a~&---> ~a~&" (tps (car i)) (cdr i))
		 (progn (format t "Modified: ~a~&" (tps (car i))) (format t (cdr i)) (format t "~&"))))
	(format t "Created: ~a~&---> ~a~&" (tps (caar (last mht))) (cdar (last mht)))))))

(defmethod history ((self list) &optional (opt #'(lambda (x) (mapcar #'reverse (if (loop for i in x always (numberp (cadr i))) (ordinate x #'< :key #'cadr) (ordinate x #'< :key #'car))))))
  (labels ((cil (lst &optional r)
	     (dolist (e (remove-duplicates lst :test #'equalp) r)
	       (push (list (count e lst :test #'equalp) e) r))))
    (if (functionp opt) (funcall opt (cil self)) (history self))))

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

(defun >data-file (path lst &key comment csv)
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
	 (if csv
	     (format stream "~{~S~^, ~}~%" (list! i))
	     (format stream "~{~S ~}~%" (list! i))))))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defgeneric >dot (self nodes)
  (:documentation "Allow to draw a graph dot to visualize all connections in the net <self> following <nodes> list.
When self is MLT, nodes has to be formed for instance as follow:
nodes = ((2 ? ? 1) nil); that means (car nodes) = tournoi with wild cards for every unknown involving (length (car nodes)) = order; and (cadr nodes) = remanence (t or nil)"))

(defmethod >dot ((self area) (nodes-lst list))
  (let* ((el (remove-duplicates (ht (arcs self) :k) :test #'(lambda (a b) (equalp a (reverse b)))))
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
  (let* ((el (ht (arcs self) :k))
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

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

(defun flat-1 (lst) (if (consp (car lst)) (apply 'append lst) lst))

(defun combx (vals n)
  "From PW: Creates all combinations of n elements from vals."
   ; ATTENTION: Not always equal to (+ n 1) per a problem in recursion ...???
  (cond
    ((<=  n 0) vals)
    (t (flat-1
	(mapcar #'(lambda (x) (mapcar #'(lambda (y) (append (list! x) (list! y))) (list! vals))) (list! (combx vals (1- n))))))))

(defun all-combinations (vals n)
  "From old PatchWork: it creates all combinations of the given list in vals with a length set in n.
Note that the length of the result is equal to n^r with n as the total number of objects and r the sample size. 
Also, 'all-combinations' is a misnomer to refers in fact to an 'all-permutations-with-repetitions'."
  (let ((n (1- n)))
    (combx vals n)))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defgeneric all-tournoi (self &key order remanence))
(defgeneric all-clique (self))
(defgeneric rnd-tournoi (self &key order remanence compute))
(defgeneric rnd-clique (self &key compute))

(defmethod all-tournoi ((self mlt) &key (order (cover-value self)) (remanence t))
  (when (and (integerp order) (> order 1))
    (if remanence
	(let ((res (if (= order (cover-value self))
		       (loop for he in (ht (trns self) :al) collect (list (cdr he) (car he)))
		       (let* ((allht (loop for value being the hash-values of (trns self) using (hash-key key) collect (list value key)))
			      (allwo (if (> order (cover-value self))
					 (remove-duplicates (loop for i in allht append (locate-tournoi self (complist (cadr i) order '?))) :test #'equalp :key #'cadr)
					 (loop for k in allht append (loop for j in (loop-wind (cadr k) order) collect (list (car k) j)))))
			      (htmp (make-hash-table :test #'equalp)))
			 (loop for i in allwo
			    do
			      (setf (gethash (cadr i) htmp)
				    (if (gethash (cadr i) htmp)
					(+ (gethash (cadr i) htmp) (car i))
					(car i))))
			 (loop for he in (ht htmp :al) collect (list (cdr he) (car he)))))))
	  (ordinate (mapcar #'list (mapcar #'float (normalize-sum (mapcar #'car res))) (mapcar #'cadr res)) #'> :key #'car))      
	(let ((res (loop for i in (all-combinations (ar-ser (length (fanaux-list self))) order) when (tournoi-p i self :arcs) collect i)))
	  (ordinate (mapcar #'list (mapcar #'float (normalize-sum (get-weight self res :remanence nil))) res) #'> :key #'car)))))

(defmethod rnd-tournoi ((self mlt) &key (order (cover-value self)) (remanence t) (compute #'rnd-weighted))
  (funcall compute (mapcar #'reverse (all-tournoi self :remanence remanence :order order))))

(defmethod all-clique ((self area))
  (let ((tmp (loop for i in (reccomb (ar-ser (car (fanaux-length self))) (mapcar #'ar-ser (cdr (fanaux-length self)))) when (clique-p i self) collect i)))
    (ordinate (mapcar #'list (mapcar #'float (normalize-sum (get-weight self tmp))) tmp) #'> :key #'car)))

(defmethod rnd-clique ((self area) &key (compute #'rnd-weighted))
  (funcall compute (mapcar #'reverse (all-clique self))))

(defun rem-sublst (sub lst)
  (let ((nl lst))
    (loop for i in (list! sub) do (setf nl (remove i nl :test #'equalp))) nl))

(defgeneric locate-cycle (nodes-lst &optional order res))
(defmethod locate-cycle ((nodes-lst list) &optional order res)
  (if (< (length nodes-lst) 3)
      (ordinate res #'> :key #'length)
      (if (equalp (caar nodes-lst) (cadar nodes-lst))
	  (locate-cycle (cdr nodes-lst) order res)
	  (let ((tmp (list (car nodes-lst))))
	    (loop for i in (cdr nodes-lst)
	       do
		 (when
		     (and
		      (not (equalp (car i) (cadr i)))
		      (node= (car tmp) i :arcs 21)
		      (loop for n in tmp never (node= n i :arcs 22))
		      (not (node= (car tmp) (car (last tmp)) :arcs 21)))
		   (push i tmp)))
	    (if (and
		 (if (and (integerp order) (> order 2))
		     (= (length tmp) order)
		     (> (length tmp) 2))
		 (node= (car tmp) (car (last tmp)) :arcs 21))
		(locate-cycle (rem-sublst tmp nodes-lst) order (cons (reverse tmp) res))
		(locate-cycle (cdr nodes-lst) order res))))))

(defmethod locate-cycle ((nodes-lst hash-table) &optional order res)
  (locate-cycle (ht nodes-lst :k) order res))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defstruct (event (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (event-label p))))) label data type al)
(defmethod id ((self event)) self)

(defun x->dx (lst)
  (loop for x in lst
     for y in (rest lst)
     collect (- y x)))

(defun dx->x (start dx)
  (let ((r (list start)))
    (loop for i in dx do (push (+ i (car r)) r))
    (reverse r)))

(defun subseq-thres (seq1 seq2 thres)
  "seq = (dx->x 0 (normalize-sum duration-list))"
  (remove nil (loop for i in seq1
		 collect (loop for j in seq2
			    when (<= (abs (- j i)) (* thres (min (caddr seq1) (caddr seq2))))
			    collect j))))

(defun filternoway (lst)
  (let ((r (list (car lst))))
    (loop for i in (cdr lst)
       do
	 (let ((tmp (member (caar r) i)))
	   (if tmp
	       (unless (null (cdr tmp)) 
                 (if (= (car (last tmp)) (car (last (car r))))
                     (let ((ir (butlast (car r))))
                       (setf r (cdr r)) 
                       (push ir r)
                       (push (cdr tmp) r))
                   (push (cdr tmp) r)))
             (push i r))))
    (reverse r)))

(defun prim (a)
  (if (zerop a) a (/ a (abs a))))

(defun mk-y (x lst) ; lst --> ((x1 y1) (x2 y2))
  (let ((x1 (caar lst)) 
	(y1 (cadar lst))
	(x2 (caadr lst))
	(y2 (cadadr lst)))
    (cond ((and (= x1 x2) (= y1 y2)) y1)
	  ((= x1 x2) (abs (- y1 y2))) ;; euclidean distance between y1 and y2
	  (t ;; linear inrerpolation
	   (let* ((b (/ (- (* x1 y2) (* x2 y1)) (- x1 x2)))
		  (a (/ (- y1 y2) (- x1 x2))))
	     (+ b (* x a)))))))

(defun dur>onset (xs/l ended)
  (let ((x (dx->x 0 (normalize-sum (car xs/l))))
	(y (cadr xs/l))
	(mv (apply #'min (normalize-sum (car xs/l)))))
    (case ended
      (:ignore (list (butlast x) y mv))
      (:first (list x (append y (list (car y))) mv))
      (:last (list x (append y (last y)) mv)))))

(defun median-group (clx cres)
  (if (null cres)
      (mean clx)
      (let (plus same minus)
	(loop for i in clx do (cond ((= 0 (prim (- i cres))) (push i same))
				    ((= 1 (prim (- i cres))) (push i plus))
				    ((= -1 (prim (- i cres))) (push i minus))))
	(let ((sl (sort (list plus same minus) #'> :key #'length)))
	  (cond ((apply #'= (mapcar #'length sl)) (mean (flatten sl)))
		((= (length (car sl)) (length (cadr sl))) (mean (flatten (butlast sl))))
		(t (mean (car sl))))))))

(defun lx->dx (lx cluster &optional res)
  (if lx
      (let ((x (if (singleton (car lx))
		   (caar lx)
		   (case cluster
		     (:mean (mean (car lx)))
		     (:maxima (apply #'max (car lx)))
		     (:minima (apply #'min (car lx)))
		     (:median (median-group (car lx) (car res)))))))
	(when res (setf res (cons (- x (car res)) (cdr res))))
	(lx->dx (cdr lx) cluster (push x res))) 
      (reverse (cdr res))))

(defgeneric differential-vector (a b &key result opt thres ended tolerance cluster)
  (:documentation
   "Returns in terms of distance the normalized norm of the vector or its coordinates:
 - <result :diff-coord> (1-x 1-y),
 - <result :diff-norm> [default] ||(1-x 1-y)||,
 - <result :sim-coord> (x y),
 - <result :sim-norm> ||(x y)||;
defined by:
x as the percentage of the timing concordances according to the three following options:
 - <opt :max> number of concordances divided by the maximal cardinal between the two sequences;
 - <opt :min> number of concordances divided by the minimal cardinal between the two sequences; 
 - <opt :mean> [default] number of concordances divided by the average of cardinals of the two sequences,
with a threshold as a percentage -- i.e. 1 = 100% -- of the minimal duration value of the sequences involved [0.2 by default];
the clustering discrimination is done according to the following keywords:
 - <cluster :maxima> retains the maximal value of the cluster,
 - <cluster :minima> retains the minimal value of the cluster,
 - <cluster :mean> compute the mean value of the cluster,
 - <cluster :median> [default] compute the mean value of the bigger group in terms of first derivative signs (in case of equality this is the mean value of the union of these groups);
y as percentage of the profile resemblance relative to the events concordances defined in x. This is computed from the sign of the first derivative of durations. The last duration can be involved as the repetition of the last level set with <ended :last> -- that means the derivative equal to zero --, according to a cyclicity by the repetition of the first level set with <ended :first>, or simply ignored set with <ended :ignore> [default].
The key tolerance concerns the primitive concordance as 1 = 0 and -1 = 0:
  - <tolerance :no> [default].
  - <tolerance :yes>.
The input xs/l has to be a list of durations and its respective profile level as follow: 
((dur-1 dur-2 ... dur-n) (val-1 val-2 ...val-n))
Note that each list of durations is scaled such as sum of durations is equal to 1 and val-i is a positive number or a list of positive numbers."))

(defmethod differential-vector ((xs/l1 list) (xs/l2 list) &key (result :diff-norm) (opt :mean) (thres 0.2) (ended :ignore) (tolerance :no) (cluster :median))
  (let* ((l1 (dur>onset xs/l1 ended))
	 (l2 (dur>onset xs/l2 ended))
	 (lx1 (filternoway (subseq-thres (car l1) (car l2) thres)))
	 (lx2 (filternoway (subseq-thres (car l2) (car l1) thres)))
	 (p1 (mapcar #'(lambda (x) (flatten (loop for i in x collect (cadr (assoc i (mat-trans (butlast l2))))))) lx1))
	 (p2 (mapcar #'(lambda (x) (flatten (loop for i in x collect (cadr (assoc i (mat-trans (butlast l1))))))) lx2))
	 (seqsort (sort (list (length (car xs/l1)) (length (car xs/l2))) #'<))
	 (x (case opt
	      (:min (/ (if (eq ended :ignore) (length lx1) (1- (length lx1))) (car seqsort) 1.0))
	      (:max (/ (if (eq ended :ignore) (length lx1) (1- (length lx1))) (cadr seqsort) 1.0))
	      (:mean (/ (if (eq ended :ignore) (length lx1) (1- (length lx1))) (mean seqsort)))))
	 (y (mapcar #'(lambda (a b)
			(case tolerance
			  (:no (if (= (prim a) (prim b)) 1 0))
			  (:yes (if (or (= 1 (abs (+ (prim a) (prim b)))) (= (prim a) (prim b))) 1 0))
			  (otherwise (error "The key :tolerance requires as arguments either the keyword :yes or :no."))))
		    (lx->dx p1 cluster) (lx->dx p2 cluster))))
    (when *debug* (format t "l1: ~S~&l2: ~S~&lx1: ~S~&lx2: ~S~&p1: ~S~&p2: ~S~&seqsort: ~S~&x: ~S~&y: ~S~&lxp1: ~S~&lxp2: ~S~&lx1nof: ~S~&lx2nof: ~S~&" l1 l2 lx1 lx2 p1 p2 seqsort x y (lx->dx p1 cluster) (lx->dx p2 cluster) (subseq-thres (car l1) (car l2) thres) (subseq-thres (car l2) (car l1) thres)))
    (case result
      (:diff-x (values (- 1 x) (/ (sqrt (+ (expt (- 1 x) 2) (expt (- 1 (mean y)) 2))) (sqrt 2))))
      (:diff-y (values (- 1 (mean y)) (/ (sqrt (+ (expt (- 1 x) 2) (expt (- 1 (mean y)) 2))) (sqrt 2))))
      (:diff-norm (values (/ (sqrt (+ (expt (- 1 x) 2) (expt (- 1 (mean y)) 2))) (sqrt 2)) (list (- 1 x) (- 1 (mean y)))))
      (:diff-coord (values (list (- 1 x) (- 1 (mean y))) (/ (sqrt (+ (expt (- 1 x) 2) (expt (- 1 (mean y)) 2))) (sqrt 2))))
      (:diff-list (list (/ (sqrt (+ (expt (- 1 x) 2) (expt (- 1 (mean y)) 2))) (sqrt 2)) (- 1 x) (- 1 (mean y))))
      (:sim-x (values x (/ (sqrt (+ (expt x 2) (expt (mean y) 2))) (sqrt 2))))
      (:sim-y (values (mean y) (/ (sqrt (+ (expt x 2) (expt (mean y) 2))) (sqrt 2))))
      (:sim-norm (values (/ (sqrt (+ (expt x 2) (expt (mean y) 2))) (sqrt 2)) (list x (mean y))))
      (:sim-coord (values (list x (mean y)) (/ (sqrt (+ (expt x 2) (expt (mean y) 2))) (sqrt 2))))
      (:sim-list (list (/ (sqrt (+ (expt x 2) (expt (mean y) 2))) (sqrt 2)) x (mean y))))))

(defmethod differential-vector ((a event) (b event) &key (result :diff-norm) (opt :mean) (thres 0.2) (ended :ignore) (tolerance :no) (cluster :median))
  (differential-vector (event-data a) (event-data b) :result result :opt opt :thres thres :ended ended :tolerance tolerance :cluster cluster))

(defmethod differential-vector ((a list) (b null) &key (result :diff-norm) (opt :mean) (thres 0.2) (ended :ignore) (tolerance :no) (cluster :median))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse r))
      (loop for j from (1+ i) to (1- (length a)) do
	   (push (list i j (differential-vector (nth i a) (nth j a) :result result :opt opt :thres thres :ended ended :tolerance tolerance :cluster cluster)) r)))))

(defmethod differential-vector ((a null) (b list) &key (result :diff-norm) (opt :mean) (thres 0.2) (ended :ignore) (tolerance :no) (cluster :median))
  (differential-vector b a :result result :opt opt :thres thres :ended ended :tolerance tolerance :cluster cluster))

;------------------------------------------------------------------
