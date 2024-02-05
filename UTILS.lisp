;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)
(defvar *debug* nil)
(defparameter *notename* '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "G#" "A" "Bb" "B"))

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
;                                        PRETTY PRINTING TABLE DATA

;; https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f
(defconstant +CELL-FORMATS+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

(defun format-table (stream data &key
				   (column-label (loop for i from 1 to (length (car data))
                                                          collect (format nil "COL~D" i)))
                                   (column-align (loop for i from 1 to (length (car data))
                                                       collect :left)))
  (let* ((col-count (length column-label))
         (strtable  (cons column-label ; table header
                          (loop for row in data ; table body with all cells as strings
				collect (loop for cell in row
                                              collect (if (stringp cell)
                                                          cell
					                  (format nil "~A" cell))))))
         (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
                                    do (setf (aref widths i)
                                             (max (aref widths i) (length cell))))
                           finally (return widths))))
    ;----------------------------------------------------
    ; splice in the header separator
    (setq strtable
          (nconc (list (car strtable) ; table header
                       (loop ; generate separator
                             for width across col-widths
                             collect (format nil "~v@{~A~:*~}" width "-")))
                 (cdr strtable))) ; table body
    ;----------------------------------------------------
    ; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
				 collect (getf +CELL-FORMATS+ align))))
          (widths  (loop for w across col-widths collect w)))
					; write each line to the given stream
      (dolist (row strtable)
        (apply #'format stream row-fmt (mapcan #'list widths row))))))

;------------------------------------------------------------------
;                                                             UTILS

(defun list! (obj) (if (listp obj) obj (list obj)))

(defun n-first (n lst) (if (> n (length lst)) lst (subseq lst 0 n)))

(defun version () (format t "~a" (asdf:component-version (asdf:find-system "n3"))))

(defun >thrifty-code (a digit &optional (key :left) (from 0)) ;; possible key are -> :left (default), :right, :mid (implies an odd digit number)
  (case key
    (:right (reverse (replace-a 1 (- a from) (make-list digit :initial-element 0))))
    (:left (replace-a 1 (- a from) (make-list digit :initial-element 0)))
    ;; in the case of :mid, the value of from=0 -> clip the exceeding values and from=1 (default) -> apply modulo +/-(floor (/ digit 2))
    (:mid (when (oddp digit) (replace-a 1 (+ (cond
					       ((and (zerop from) (> a (floor (/ digit 2)))) (floor (/ digit 2)))
					       ((and (zerop from) (< a (* -1 (floor (/ digit 2))))) (* -1 (floor (/ digit 2))))
					       ((and (= 1 from) (> a (floor (/ digit 2)))) (mod a (floor (/ digit 2))))
					       ((and (= 1 from) (< a (* -1 (floor (/ digit 2))))) (mod a (* -1 (floor (/ digit 2)))))
					       (t a)) (floor (/ digit 2))) (make-list digit :initial-element 0))))))

(defun winner-take-all (lst &optional (ind 0) tmp) ;; the result is the position of the 'winner' in lst
  (let ((wta (loop for x in (if tmp tmp lst) for i from 0 when (= x (reduce #'max (if tmp tmp lst))) collect i))) 
       (if (singleton wta)
	   (car wta)
	   (if (< ind (length lst))
	       (winner-take-all
		lst
		(1+ ind)
		(loop for x in (butlast (loop for i from 0 to (length lst) collect (subseq lst (if (< (- i ind) 0) 0 (- i ind)) (if (> (+ ind i 1) (length lst)) (length lst) (+ ind i 1))))) for pos from 0 collect (if (member pos wta) (reduce #'+ x) 0)))
	       (nth (random (length wta)) wta)))))

(defun <thrifty-code (lst &optional (key :left) (from 0))
  (case key
    (:right (+ (1- from) (- (length lst) (winner-take-all lst))))
    (:left (+ (winner-take-all lst) from))
    (:mid (- (winner-take-all lst) (floor (/ (length lst) 2))))))

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

(defgeneric the-ds (self) (:method ((self mlt)) (cdar (ordinate (loop for i in (ht (date-report self) :al) when (ds-p (cdr i)) collect i) '> :key #'car))))
(defun addtothird (n lst) (cond ((= 2 (length lst)) (reverse (cons (if n n 0) (reverse lst)))) ((= 3 (length lst)) (reverse (cons (if n n 0) (reverse (butlast lst))))) (t nil)))

(defgeneric set-ds (self &rest ds)
  (:documentation "DS [as key + value] {default-value} 
+-----------------------------------------------------------------------+
|    :norm (min max curve{0}) or curve                                  |
| OR :dim ((min[1] max[1] curve[1]{0}) ... (min[n] max[n] curve[n]{0})) |               
+-----------------------------------------------------------------------+
Note that the output is clipped if the range of input is largest than values of :norm or :dim
+-----------------------------------------------------------------------+
|    :bypass [as identity set by default]                               | 
| OR :bypass (lambda* (x) ...)                                          |
+-----------------------------------------------------------------------+")
  (:method ((self mlt) &rest ds)
    ;; test ds
    (if (or (and (eq :bypass (car ds)) (or (null (cadr ds)) (ml? (cadr ds))))
	    (and (eq :norm (car ds)) (listp (cadr ds)) (or (= 2 (length (cadr ds))) (= 3 (length (cadr ds)))) (loop for n in (cadr ds) always (numberp n)) (null (caddr ds)))
	    (and (eq :dim (car ds)) (listp (cadr ds)) (loop for x in (cadr ds) always (and (listp x) (or (= 2 (length x)) (= 3 (length x))) (loop for y in x always (numberp y)))) (null (caddr ds))))
	;; add DS to history of self
	(if (equalp (dt (the-ds self)) ds)
	    (the-ds self)
	    (let ((ct (get-universal-time)))
	      (setf (gethash ct (date-report self)) (make-instance 'ds :dt ds))
	      (the-ds self)))
    (warn "Wrong argument(s)! Check your DS entry..."))))

(defgeneric zero-a (x &optional quasi-zero)
 (:documentation "Replace all quasi-zero by zero.")
 ;; zero-a is [...] a kind of deleuzian un-differentiation...
 (:method ((x number) &optional (quasi-zero .00001)) (if (< (abs x) quasi-zero) 0.0 x))
 (:method ((x list) &optional (quasi-zero .00001)) (mapcar #'(lambda (a) (zero-a a quasi-zero)) x)))

(defgeneric scaling (data &key minin maxin minout maxout curve mlt norm dim bypass update))
(defmethod scaling ((data number) &key minin maxin minout maxout curve mlt norm dim bypass update)
  (declare (ignore norm dim))
  (if (mlt-p (id mlt))
      (cond ((and bypass (ml? bypass)) (when update (apply #'set-ds (cons (id mlt) (list :bypass bypass)))) (funcall bypass data))
	    ((ml? (cadr (list! (dt (the-ds (id mlt)))))) (funcall (cadr (dt (the-ds (id mlt)))) data))
	    (t data))
      (if (and minin maxin minout maxout curve)
	  (if (> (abs curve) 0.001)
	      ; i.e. SuperCollider method .curvelin
	      (let* ((grow (exp curve))
		     (a (/ (- maxin minin) (- 1 grow)))
		     (b (+ minin a)))
		(zero-a (+ minout (/ (* (log (/ (- b (cond ((> data maxin) maxin) ((< data minin) minin) (t data))) a)) (- maxout minout)) curve))))
	      (zero-a (+ minout (/ (* (- (cond ((> data maxin) maxin) ((< data minin) minin) (t data)) minin) (- maxout minout)) (- maxin minin)))))
	  data)))

(defmethod scaling ((data null) &key minin maxin minout maxout curve mlt norm dim bypass update)
  (declare (ignore data minin maxin minout maxout curve norm dim bypass update))
  (if (mlt-p (id mlt)) (make-list (nbre-input (id mlt)) :initial-element 0) 0))

(defmethod scaling ((data list) &key minin maxin minout maxout curve mlt norm dim bypass update)
  (if (mlt-p (id mlt))
      ;;-------------------------------
      (let (ddts)
	(cond ((ml? bypass)
	       (when *debug* (format t ":bypass & lambda*"))
	       (setf ddts (list :bypass bypass)))
	      (bypass
	       (when *debug* (format t ":bypass"))
	       (setf ddts (list :bypass)))
	      ((and (not norm) (not dim) (not bypass))
	       (when *debug* (format t ":bypass [default]"))
	       (setf ddts (list! (dt (the-ds (id mlt))))))
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
	(when update (apply #'set-ds (cons (id mlt) ddts)))
	(case (car ddts)
	  (:bypass (if (ml? (cadr ddts)) (funcall (cadr ddts) data) data))
	  (:norm (mapcar #'(lambda (x) (scaling x :minin (car (cadr ddts)) :maxin (cadr (cadr ddts)) :minout 0 :maxout 1 :curve (caddr (cadr ddts)))) data))
	  (:dim (mat-trans (loop for d in (mat-trans data) for s in (cadr ddts) collect (scaling d :minin (car s) :maxin (cadr s) :minout 0 :maxout 1 :curve (caddr s)))))))
      ;;-------------------------------
      (mapcar #'(lambda (x) (scaling x :minin (if minin minin (reduce #'min (flatten data))) :maxin (if maxin maxin (reduce #'max (flatten data))) :minout (if minout minout 0) :maxout (if maxout maxout 1) :curve (if curve curve 0))) data)))

;;;  ;  ;;  ; ; ;; ; ; ; ;   ;

(defgeneric mapping (self iteration dataset &key init-lr init-rad end-lr end-rad init-ep df ds))
(defmethod mapping ((self som) (iteration integer) (dataset list) &key (init-lr 0.1) (init-rad (/ (funcall #'mean (list! (field self))) 2)) (end-lr 0.01) (end-rad 0.1) (init-ep (epoch self)) (df #'exp-decay) (ds (dt (the-ds self))))
  (let ((data (if (ml? (eval (nth (1+ (position :bypass ds)) ds)))
		  (loop for dat in dataset collect (eval (append (list 'funcall #'scaling dat :mlt self) (list! ds))))
		  (apply #'scaling (cons dataset (append '(:mlt self) (list! ds)))))))  
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
		 (progn (format t "Modified: ~a~&---> " (tps (car i))) (format t (cdr i)) (format t "~&"))))
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
	(if (listp val)
	    (loop for i in val do 
	      (when (and (integerp i) (<= i (length items)) (> i 0)) (load-neural-network (string (car (split-symbol (nth (- i 1) items)))))))
	    (when (and (integerp val) (<= val (length items)) (> val 0)) (load-neural-network (string (car (split-symbol (nth (- val 1) items)))))))))))

;;;; ;; ;;  ; ;;;  ;; ;  ; ; ; ;   ;

(defun convert-list-to-array (lst)
  (with-output-to-string (stream)
    (uiop:run-program (concatenate 'string "echo '" (format nil "~a" lst) "' | sed -e 's/\\ /, /g;s/(/[ /g;s/)/ ]/g' | awk '{print tolower(\$0)}'") :output stream)))

(defun >data-file (path lst &key append out)
  "Allows to write data file (lst) to a file (path)."
  (let ((file (make-pathname
	       :directory (pathname-directory path)
	       :name (pathname-name path)
	       :type (pathname-type path))))
    (with-open-file (stream file
			    :direction :output
			    :if-exists (if append :append :supersede)
			    :if-does-not-exist :create)
      (loop for i in lst
	    do
	       (case out
		 (:csv (format stream "~{~S~^, ~}~%" (list! i)))
		 (:sc (format stream "~a" (convert-list-to-array (list! i))))
		 (otherwise (format stream "~{~S ~}~%" (list! i))))))))

(defun df2sc (file &optional name)
  "Convert <file> generated with (>data-file <path> <data> :out :sc) to a valid SuperCollider file,
which can be interpreted as a global variable defind by <name> prepended with a tilde."
  (let ((scdfile (namestring (make-pathname :directory (pathname-directory file) :name (if name (string name) (pathname-name file)) :type "scd"))))
    (uiop:run-program (concatenate 'string "paste -s -d, " (namestring file) " | sed -e '1s/^/(~" (if name (string name) (pathname-name file)) " = [/' > " scdfile " ; echo '])' >> " scdfile))))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defgeneric >dot (self nodes)
  (:documentation "Allow to draw a graph dot to visualize all connections in the net <self> following <nodes> list.
When self is MLT, nodes has to be formed for instance as follow:
nodes = ((2 ? ? 1) nil); that means (car nodes) = tournoi with wild cards for every unknown involving (length (car nodes)) = order; and (cadr nodes) = remanence (t or nil)"))

(defmethod >dot ((self area) (nodes-lst list))
  (let* ((el (remove-duplicates (ht (arcs self) :k) :test #'(lambda (a b) (equalp a (reverse b)))))
	 (ed (loop for i in nodes-lst collect (loop for j in el when (or (equalp i (car j)) (equalp i (cadr j))) collect j)))
	 (cil (history (flat-once (flat-once ed))))
	 (ped (loop for i in cil when (and (not (member (car i) nodes-lst :test #'equalp)) (>= (cadr i) (length nodes-lst))) collect (car i)))
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

(defun standard-deviation (lst)
  (sqrt (/ (apply #'+ (mapcar #'(lambda (x) (expt (- x (mean lst)) 2)) lst)) (length lst))))

(defgeneric locate-cycle (nodes-lst &optional kw order res))
(defmethod locate-cycle ((nodes-lst list) &optional kw order res)
  ;;------------------------------------
  ;; hack to detect cycle in a list and return the cycle if so ...
  (if (eq kw :in-list)
      (loop for i from 1 to (ceiling (/ (length nodes-lst) 2))
	    until (loop for a in nodes-lst for b in (nthcdr i nodes-lst) always (equalp a b))
	    finally (when (<= i (floor (/ (length nodes-lst) 2))) (return (subseq nodes-lst 0 i))))
  ;;------------------------------------
      (if (< (length nodes-lst) 3)
	  (ordinate res #'> :key #'length)
	  (if (equalp (caar nodes-lst) (cadar nodes-lst))
	      (locate-cycle (cdr nodes-lst) kw order res)
	      (let ((tmp (list (car nodes-lst))))
		(loop for i in (cdr nodes-lst)
		      do
			 (when
			     (and
			      (not (equalp (car i) (cadr i)))
			      (node= (car tmp) i :arcs 21)
			      (loop for n in tmp never (node= n i :arcs 22))
			      (not (node= (car tmp) (carlast tmp) :arcs 21)))
			   (push i tmp)))
		(if (and
		     (if (and (integerp order) (> order 2))
			 (= (length tmp) order)
			 (> (length tmp) 2))
		     (node= (car tmp) (carlast tmp) :arcs 21))
		    (locate-cycle (rem-sublst tmp nodes-lst) kw order (cons (reverse tmp) res))
		    (locate-cycle (cdr nodes-lst) kw order res)))))))

(defmethod locate-cycle ((nodes-lst hash-table) &optional kw order res)
  (if kw
      (let ((tmp (locate-cycle (ht nodes-lst :k) kw order res)))
	(values
	 tmp
	 (case kw
	   ;; list of weight's edges by cycle
	   (:wlist (mapcar #'(lambda (cycle) (loop for i in cycle collect (cdr (assoc i (ht nodes-lst :al) :test #'equalp)))) tmp))
	   ;; (ratio mean std-dev)
	   ;; ratio is the sum of weight's edges by cycle
	   ;; mean is the mean value of the normalized wlist -- real value is (* ratio mean)
	   ;; std-dev is the standard deviation of the normalized wlist -- real value is (* ratio std-dev)
	   (:wstats (let ((ns (mapcar #'(lambda (cycle) (loop for i in cycle collect (cdr (assoc i (ht nodes-lst :al) :test #'equalp)))) tmp)))
		      (mat-trans (list (mapcar #'sum ns) (mapcar #'mean (mapcar #'normalize-sum ns)) (mapcar #'standard-deviation (mapcar #'normalize-sum ns))))))
	   (otherwise (warn "Keyword not recognized!")))))
      (locate-cycle (ht nodes-lst :k) kw order res)))

(defmethod locate-cycle ((nodes-lst mlt) &optional kw order res)
  (locate-cycle (arcs nodes-lst) kw order res))

(defmethod locate-cycle ((nodes-lst area) &optional kw order res)
  (locate-cycle (arcs nodes-lst) kw order res))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defstruct (event (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (event-label p))))) label data type al)
(defmethod id ((self event)) self)

(defmethod euclidean ((arg1 event) (arg2 event) &key modulo position weight)
  (euclidean (event-data arg1) (event-data arg2) :modulo modulo :position position :weight weight))

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
                 (if (= (carlast tmp) (carlast (car r)))
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

;------------------------------------------------------------------

