;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;; write file
(defun write-file (data &key path name head tail)
  "data has to be a list of objects corresponding to the lines of the file.
The head and the tail are - if set - displayed on a single line, respectively before and after the data set.
If needed add newline with #\Space in the data set."
  (with-open-file (stream (make-pathname :directory (pathname-directory (if (= 0 (length path)) (user-homedir-pathname) path))
					 :name (pathname-name (if name (string name) (if (or (= 0 (length path)) (= 0 (length (pathname-name path)))) "untitled" path)))
					 :type (pathname-type (if name (string name) (if (or (= 0 (length path)) (= 0 (length (pathname-name path)))) "" path))))
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (when head (format stream "~{~A ~}~&" (if (listp head) head (list head)))) 
    (loop for i in (butlast data) do (format stream "~{~A ~}~&" (if (listp i) i (list i))))
    (loop for i in (last data) do (format stream "~{~A ~}" (if (listp i) i (list i))))
    (when tail (format stream "~&~{~A ~}" (if (listp tail) tail (list tail))))))

;;==================================================================
;;                     CONTRASTIVE ANALYSIS
;;==================================================================
(defun get-all-peak (data)
  (let ((ref (loop for i from 1 to (1- (length data)) collect (list (car (nth i data)) (- (cadr (nth i data)) (cadr (nth (1- i) data)))))))
    (loop for i from 0 to (- (length ref) 2) when (and (> (cadr (nth i ref)) 0) (< (cadr (nth (1+ i) ref)) 0)) collect (nth i ref))))
;;------------------------------------------------------------------
;; generate png with gnuplot from tree data  
(defmethod gnuplot> ((datapath string) &key mlt (w 1200) (h 600) (scale 1) (fontsize 15) (gnuplot *gnuplot*))
  (let* ((data (read-file datapath))
	 (dir (directory-namestring datapath))
	 (dp (get-all-peak data))
	 (omin (apply #'min (mapcar #'cadr dp)))
	 (omax (apply #'max (mapcar #'cadr dp)))
	 (al (mapcar #'caddr data))
	 (sc (mapcar #'(lambda (x) (+ omin (* (/ (- omax omin) (- (apply #'max al) (apply #'min al))) (- x (apply #'min al))))) al))) 
    (write-file dp :name "dist-peaks" :path datapath)
    (write-file (loop for i from 0 to (1- (length data)) collect (list (car (nth i data)) (nth i sc))) :name "scaled-inertia" :path datapath)
    (let ((dat (list
		(format nil "set terminal png size ~S,~S font \"Times, ~S\"" (round (* scale w)) (round (* scale h)) fontsize)
		(format nil "set output '~A~A.png'" (directory-namestring datapath) (pathname-name datapath))
		(format nil "unset xtics~&unset ytics~&unset border~&set multiplot~&set tmargin 2~&")
		(format nil "plot \"~Ascaled-inertia\" using 1:2 title '' with lines~&" dir)
		(format nil "set logscale y 10~&set bmargin 1~&set tmargin 6~&")
		(format nil "plot \"~Adist-peaks\" using 1:2 title '~A' with impulses, '' using 1:2:(sprintf(\"%.0f [%.3f]\",$1,$2)) with labels rotate by 90 offset character 0,3 notitle" dir (if (mlt-p mlt) (name mlt) (if mlt mlt ""))))))
      (write-file dat :name "gnuplot.pl" :path datapath)))
  (UIOP::run-program (format nil "sh -c '~S ~S'" gnuplot (concatenate 'string (directory-namestring datapath) "gnuplot.pl"))))

(defgeneric open-graph (self &key mlt w h scale fontsize gnuplot display))
(defmethod open-graph ((self node) &key mlt (w 1200) (h 600) (scale 1) (fontsize 15) (gnuplot *gnuplot*) (display *display*))
  (declare (ignore mlt))
  (let ((path (format nil "~A~A/~A/~A.dat" *N3-BACKUP-DIRECTORY* (car (node-data self)) (car (last (node-data self))) (car (node-data self)))))
    (if (probe-file path)
	(progn 
	  (gnuplot> path :w w :h h :scale scale :fontsize fontsize :gnuplot gnuplot)
	  (UIOP::run-program (format nil "sh -c '~S ~S'" display (concatenate 'string (directory-namestring path) (car (node-data self)) ".png"))))
	(warn "This tree does not have any data file, to add it use the function dendrogram with the key :and-data set as t."))))
;;------------------------------------------------------------------
(defgeneric trim-tree (self tree n-class))
(defmethod trim-tree ((self som) (tree node) (n-class integer))
  "Built a list of sublist of leaves according a given number of classes from the CAH. 
For now tree has to be the node root."
  (declare (ignore self))
  (let ((nodes (car (assoc n-class (history tree) :key #'length))))
    (loop for i in nodes collect
	     (mapcar #'output (mapcar #'id (mapcar #'node-data (get-leaves i)))))))

(defparameter *letters* '(A B C D E F G H I  J K L M N O P Q R S T U V W X Y Z))
(defun int2letter (int &optional res)
  (cond ((zerop int) (apply #'concatenate (cons 'string res)))
	((zerop (cadr (multiple-value-list (floor int 26)))) (int2letter (1- (floor (/ int 26))) (push "Z" res)))
	(t (int2letter (floor (/ int 26)) (push (write-to-string (nth (cadr (multiple-value-list (floor (1- int) 26))) *letters*)) res)))))
;;------------------------------------------------------------------
;; expand/compress allow to manage original sequence and 'smoothed' sequence (i.e. local duplicates removed)
(defgeneric expand (alphanum &key root))
(defmethod expand ((alphanum string) &key root)
  (let* ((str (string alphanum))
	 (pos (position-if #'digit-char-p str))
	 (label (if pos (subseq str 0 pos) str))
	 (substr (when pos (subseq str pos)))
	 (n (if pos (subseq substr 0 (position-if-not #'digit-char-p substr)) "1")))
    (if root (read-from-string label)
	(make-list (read-from-string n) :initial-element (read-from-string label)))))

(defmethod expand ((alphanum symbol) &key root)
  (let* ((str (string alphanum))
	 (pos (position-if #'digit-char-p str))
	 (label (if pos (subseq str 0 pos) str))
	 (substr (when pos (subseq str pos)))
	 (n (if pos (subseq substr 0 (position-if-not #'digit-char-p substr)) "1")))
    (if root (read-from-string label)
	(make-list (read-from-string n) :initial-element (read-from-string label)))))

(defmethod expand ((seq list) &key root)
  (let ((sq (loop for i in seq collect (expand i :root root))))
    (if root sq (flatten sq))))

(defgeneric compress (in &optional a b)) 
(defmethod compress ((x symbol) &optional n foo)
  (declare (ignore foo))
  (if (and (integerp n) (> n 1)) (read-from-string (concatenate 'string (string x) (write-to-string n))) x))

(defmethod compress ((alphaseq list) &optional r s)
  (if (or r s)  
      (if (equalp (car alphaseq) (car s))
	  (compress (cdr alphaseq) r (push (car alphaseq) s))
	  (compress (cdr alphaseq)
		   (if s (cons (compress (car s) (length s)) r) r)
		   (list (car alphaseq))))
      (compress (cdr alphaseq) r (cons (car alphaseq) s))))

(defmethod compress ((a null) &optional r s)
  (nreverse (if s (cons (compress (car s) (length s)) r) r)))

(defun rem-local-dup (lst)
  (expand (compress lst) :root t))
;;------------------------------------------------------------------
;; structural grouping
(defun list-module (a &optional one)
  (let ((r nil))
    (dotimes (i a (if one (mapcar #'1+ (reverse r)) (reverse r))) (push i r))))

(defun mk-int-lst (lstIn short-lst)
  (loop for i in lstIn
        collect (cadr (assoc i (mapcar #'list short-lst (list-module (length short-lst))) :test #'equalp))))  

(defun rms (s)
  (let ((sm (loop for i from 0 to (1- (length (string s))) collect (subseq (string s) i (1+ i)))))
    (read-from-string (apply #'concatenate 'string (mapcar #'int2letter (mapcar #'1+ (mk-int-lst sm (remove-duplicates sm :from-end t :test #'equalp))))))))

(defun merge-head (as &optional end)
  (if end
      (progn
	(setf end (list (car as)))
	(loop for i in (cdr as)
	   do
	     (let ((a (string (car end)))
		   (b (string i)))
	       (if (and (>= (length b) (length a)) (string= a b :end2 (length a)))
		   (setf end (cons (read-from-string (concatenate 'string a b)) (cdr end)))
		   (push (read-from-string b) end))))
	(reverse end))
      (cons (read-from-string (concatenate 'string (string (car as)) (if (null (cadr as)) "" (string (cadr as))))) (nthcdr 2 as))))

(defun merge-tail (as &optional end)
  (setf end (list (car as)))
  (loop for i in (cdr as)
	   do
	     (let ((a (string (car end)))
		   (b (string i)))
	       (if (and (>= (length a) (length b)) (string= a b :start1 (- (length a) (length b))))
		   (setf end (cons (read-from-string (concatenate 'string a b)) (cdr end)))
		   (push (read-from-string b) end))))
  (reverse end))

(defun carchar (s &optional (l 1))
  (when s (read-from-string (subseq (string s) 0 (if (> l (length (string s))) 1 l)))))
  
(defun merge-local (sl &optional r s)
  (loop for i from 0 to (1- (length sl))
     do
       (if (or (equalp (car r) (nth i sl)) (equalp (car r) (carchar (nth i sl) (length (string (car r))))))
	   (push (nth i sl) r)
	   (progn
	     (when r (push (read-from-string (apply #'concatenate 'string (mapcar #'string (reverse r)))) s))
	     (setf r (list (nth i sl))))))
  (reverse (cons (read-from-string (apply #'concatenate 'string (mapcar #'string (reverse r)))) s)))

(defparameter *ml* nil)
(defun sorting (as)
  (let* ((al (ordinate (mapcar #'reverse (history as)) #'> :key #'car))
	 (fal (caar al))
	 (res (loop for i in al when (= (car i) fal) collect i)))
    (car (ordinate res '< :key #'(lambda (x) (length (string (cadr x))))))))	 

(defun imark (as) ;; as = alpha sequence
  (let* ((m (sorting as))
	 (cas (compress as))
	 (il (loop for a in cas for i from 0
	       when (equalp (cadr m) (expand a :root t))
		collect i))
	 (anacrouse (if (zerop (car il)) nil (expand (subseq cas 0 (car il))))))
    (setf *ml* (car m))
    ;; (print "-----------------------------")
    ;; (loop for i from 0 to (- (length il) 1) do (print (expand (subseq cas (nth i il) (nth (1+ i) il)))))
    (flatten (append anacrouse (loop for i from 0 to (- (length il) 1) collect (merge-head (expand (subseq cas (nth i il) (nth (1+ i) il)))))))))
  
(defun structure-s (as &key (result :last) primitive)
  "-------------------------------------- 
     as ---> list of symbols
:result
:extend ---> return all steps
  :last ---> return only the last step
:primitive
      t ---> remove duplicates"
  (unless (listp (car as)) (setf as (list as))) 
  (let ((res (imark (car as))))
    (if (or (= 1 *ml*) (equalp res (car as)) (= 1 (- (length (car as)) (length res))))
	(if (= 1 *ml*)
	    (let ((res (merge-tail (merge-head (car as) t))))
	      (cond ((eql :extend result) (if primitive (loop for a in (cons res (cdr as)) collect (remove-duplicates (mapcar #'rms a))) (cons res as)))
		    ((eql :last result) (if primitive (remove-duplicates (mapcar #'rms res)) res))
		    (t (warn "Undefined key!"))))
	    (structure-s (cons (merge-local res) (cdr as)) :result result :primitive primitive))
	(structure-s (cons res as) :result result :primitive primitive))))

;; (defvar foo '(C C C A A C C D D A C B C A A E C D D A C A A B B B B B B E B B B D A C A A E B B B B B E E B B B B A C D E A A E A A C B B B B E A A A A B B A D A B C C A C E C D D A C D B A E C D A A A B B E A A E B A D A E D E A A E A D D C C B E A B B C E B D A B D A A E B A A A B B E A A B C D B B))

;;==================================================================
;;                    PARADIGMATIC ANALYSIS
;;==================================================================
(defvar *verbose* nil)
(defvar *memcache* nil)
(defgeneric string2list (in))
(defmethod string2list ((str string))
  (mapcar #'read-from-string (loop for i from 0 to (1- (length str)) collect (subseq str i (1+ i)))))

(defmethod string2list ((sym symbol))
  (string2list (string sym)))

(defmethod string2list ((in list))
  (mapcar #'string2list in))

(defun list2string (lst)
  (apply #'concatenate 'string (mapcar #'string lst)))
;;------------------------------------------------------------------
(defun remove-once (e lst &optional r s)
  (loop for i in lst do (if (and (null s) (eq e i)) (setf s t) (push i r)))
  (reverse r))

(defun serial-intersection (a b)
  (let* ((sm (if (> (length a) (length b)) b a))
	 (l1 (string2list sm))
	 (l2 (if (eq sm a) (string2list b) (string2list a)))
	 (ll2 l2)
	 r s rr)
    (loop for i in l1 when (member i l2) do (push i r) (setf l2 (remove-once i l2)))
    (setf rr (reverse r))
    (loop for j in ll2 when (member j r) do (push j s) (setf r (remove-once j r) l1 (remove-once j l1)))
    (list rr (reverse s))))
;;------------------------------------------------------------------
(defun if-compress? (a)
  (position-if #'digit-char-p (if (stringp a) a (string a))))

(defun decr-seq (seq x)
  ;; search and replace x<n> in seq by x<n-1>
  (let* ((al (loop for i in seq for pos from 0 if (eq x (expand (compress i) :root t)) collect (list (length (expand i)) pos)))
	 (pos (cadr (assoc (loop for i in al maximize (car i)) al))))
    (substitute-nth (car (compress (cdr (expand (nth pos seq))))) pos seq)))

(defun main-decr (a b x &optional (n 0))
  (let ((r1 (car (loop for i in a when (and (if-compress? i) (eq x (expand (compress i) :root t))) collect (decr-seq a x))))
	(r2 (car (loop for i in b when (and (if-compress? i) (eq x (expand (compress i) :root t))) collect (decr-seq b x)))))
    (if (and r1 r2)
	(main-decr r1 r2 x (1+ n))
	(list a b n))))

;; add option root t ...
(defun repetition-distance (str1 str2 &optional (w (/ 1 2)) (n 0) (al (remove-duplicates (car (serial-intersection str1 str2)))))
  (if (null al)
      (progn
	(push (list str1 str2) *memcache*)
	(when *verbose* (format t "rep to memchache = ~S = ~S~&" (list str1 str2) (* w n)))
	(* w n))
      (let ((r (main-decr (compress (string2list str1)) (compress (string2list str2)) (car al))))
	(repetition-distance (list2string (expand (car r))) (list2string (expand (cadr r))) w (+ (caddr r) n) (cdr al)))))
;;------------------------------------------------------------------
(defun factor (n)
  "Return a list of factors of n."
  (when (> n 1)
    (loop with max-d = (isqrt n)
	  for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	  (cond ((> d max-d) (return (list n))) ; n is prime
		((zerop (rem n d)) (return (cons d (factor (truncate n d)))))))))

(defun drop-element (e set)
  (cond ((null set) '())
	((equal e (first set)) (rest set))
	(t (cons (first set) (drop-element e (rest set))))))   
                      
(defun complementary (subset set)
  (cond ((null subset) set)
	((member (first subset) set)
	 (complementary (rest subset) (drop-element (first subset) set)))
	(t (complementary (rest subset) set))))

(defun 2list (l)
  (mapcar #'list (list-module (length l) t) l))

(defun boucle (l) ;l = (2list l)
  (if (null l) nil
    (let ((r (list (car l)))) 
      (loop until (assoc (cadr (car r)) r)
            do
            (push (assoc (cadr (car r)) l) r)) (reverse r))))

(defun rem-assoc (l1 l2)
  (let (r)
    (loop for i in (complementary (mapcar #'cadr l1) (mapcar #'cadr l2))
          do
          (push (assoc i l2) r)) r))

(defun c-f-p (l &optional r)
  (let ((b (boucle l)))
    (push (mapcar #'car b) r)
    (if (not (rem-assoc b l)) (reverse r)
	(c-f-p (reverse (rem-assoc b l)) r))))

(defun cfp1 (lst)
  (when (equalp (list-module (length lst) t) (ordinate lst #'<))
    (let ((r) (s (mapcar 'list (list-module (length lst) t) (ordinate lst #'<))))
      (loop for i in (c-f-p (2list lst))
            do
            (push 
             (let (z)
               (dolist (e i (mapcar #'cadr (reverse z)))
                 (push (assoc e s) z))) r)) (reverse r))))

(defun get-max-expt (a lst)
  (apply #'max (loop for i in lst collect (length (loop for j in i when (= j a) collect j)))))

(defun ppcm-c (lst)
  (let* ((c (mapcar #'length (cfp1 lst)))
         (cc (mapcar #'(lambda (x) (if (null x) '(1) x)) (loop for i in c collect (factor i))))
         (d (ordinate (remove-duplicates (flatten cc)) #'<)))
    (apply #'* (mapcar #'* d (loop for i in d collect (get-max-expt i cc))))))
    
(defgeneric dpo (a b))

(defmethod dpo ((a list) (b list))
  (abs (- (ppcm-c a) (ppcm-c b))))

(defmethod dpo ((a list) (b null))
  (assert (loop for k in a always (listp k)))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse r))
      (loop for j from (1+ i) to (1- (length a)) do
	 (push (list i j (dpo (nth i a) (nth j a))) r)))))

(defun alpha2serialint (a b)
  (let ((al (loop for i in (list-module (length a) t) for j in a collect (list j i)))
	(r nil))
    (loop for i in b do (push (assoc i al) r) (setf al (remove (assoc i al) al :test #'equalp)))
    (reverse (mapcar #'cadr r))))

(defun transposition-distance (a b &optional (w (/ 1 2)))
  (when *verbose* (format t "trans = ~S (~S ~S)~&" (if (and a b) (* w (dpo (list-module (length a) t) (alpha2serialint a b))) 0) a b))
  (if (and a b) (* w (dpo (list-module (length a) t) (alpha2serialint a b))) 0))
;;------------------------------------------------------------------
(defun levenshtein-distance (str1 str2 &optional (w 1))
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (when *verbose* (format t "edit = (~S ~S)~&" str1 str2))
  (let ((n (length str1))
        (m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
          ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
          (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
        (setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
        (setf (svref col 0) (1+ i))
        (dotimes (j m)
          (setf (svref col (1+ j))
                (min (1+ (svref col j))
                     (1+ (svref prev-col (1+ j)))
                     (+ (svref prev-col j)
                        (if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
        (rotatef col prev-col))
      (* w (svref prev-col m)))))
;;------------------------------------------------------------------
(defun structure-distance (a b &key root (wr (/ 1 2)) (wt (/ 1 2)) (wl 1))
  (setf *memcache* nil)
  (let ((aa (if (stringp a) a (string a)))
	(bb (if (stringp b) b (string b))))
    (+
     (repetition-distance aa bb wr)
     ;; has to be applied -- as root if root t -- after repetition-distance -- see (car *memcache*) -- on common items as bijective relationship
     (apply #'transposition-distance (append (apply #'serial-intersection (car *memcache*)) (list wt)))
     ;; has to be done on (car *memcache*) with the transposition pattern removed and expand -- if not root -- after transposition-distance.
     (apply #'levenshtein-distance (append (mapcar #'list2string (if root (mapcar #'rem-local-dup (mapcar #'complementary (apply #'serial-intersection (car *memcache*)) (mapcar #'string2list (car *memcache*)))) (mapcar #'complementary (apply #'serial-intersection (car *memcache*)) (mapcar #'string2list (car *memcache*))))) (list wl))))))
;;------------------------------------------------------------------
;; CAH with sub-structure as leave called event

(defvar +alist+ nil) 

(defmethod gravity-center ((lst list) (ghost event)) ;; lst --> list of events
  (let ((dat (loop for i in lst collect (event-data (id i)))))
    (setf (event-data ghost) (if (loop for i in dat always (listp i)) (mapcar #'mean (mat-trans dat)) (mean dat)))))

(defmethod dendrogram ((self list) (aggregation integer) &key (diss-fun #'structure-distance) newick with-label (and-data t))
  ;; self is a list of objects
  ;; key :diss-fun set a function applicable to the objects of self -- defind as a and b if lambda function (use macro lambda* to keep track -- see package.lisp)
  ;; key :newick to name the output file (string or symbol)
  ;; key :and-data to save tree (boolean) or an alist of symbol or string matching respectively the list self such as |self|=|and-data|
  (unless (and (= aggregation 3) (symbolp (car self)))
    (setf
     +alist+ (cond ((loop for i in self always (numberp i)) (mapcar #'write-to-string self))
		   ((and (listp and-data) (= (length and-data) (length self))) (mapcar #'string self)))
     +GA+ (make-event :label '+GA+ :type (when (symbolp (car self)) 'symbol))
     +GB+ (make-event :label '+GB+ :type (when (symbolp (car self)) 'symbol))
     *event* (format nil "~A~A/~A-~A" *N3-BACKUP-DIRECTORY* "structure" aggregation (if newick (string newick) "untitled"))
     *tree* '())
    (loop for e in self for i from 27 do (push (setf (symbol-value (intern (format nil "~S+" (read-from-string (int2letter i))))) (make-node :label (read-from-string (int2letter i)) :data (make-event :label (read-from-string (int2letter i)) :data e :type (cond (+alist+ 'alist) ((symbolp e) 'symbol) (t 'data)) :al (if +alist+ (nth (- i 27) +alist+) (int2letter i))))) *tree*))
    (dendro *tree* aggregation diss-fun)
    (setf (node-data (car *tree*)) (list (format nil "*EVENT*") aggregation
					 (let ((mvl (multiple-value-list (function-lambda-expression diss-fun))))
					 (cond
					   ((listp (car (last mvl))) (format nil "~S" (if (ml? diss-fun) (source-of diss-fun) diss-fun)))				
					   (t (format nil "~S" (car (last mvl))))))
					 (format nil "~S~S" aggregation (car *tree*))))
    (when and-data (save (car *tree*)))
    
    (with-open-file (stream (make-pathname :directory (pathname-directory *N3-BACKUP-DIRECTORY*)
					   :name ".tmp"
					   :type "nw")
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (SETF (READTABLE-CASE *READTABLE*) :PRESERVE) ;; hack to preserve case in string alist
      (format stream "~S" (list (tree>nw (car *tree*) :with-label with-label)))
      (SETF (READTABLE-CASE *READTABLE*) :UPCASE)) ;; retrieve previous state
    (UIOP::run-program (format nil "sh -c '~S ~S ~S'" (format nil "~Abin/raw2nw" *NEUROMUSE3-DIRECTORY*) (format nil "~A.tmp.nw" *N3-BACKUP-DIRECTORY*) (concatenate 'string *event* ".nw")))
    (setf
     *event* nil
     *tree* (car *tree*))))
;;------------------------------------------------------------------
;;(dendrogram '(EEEB BEEC CBEAEB BAEC CBEBDD DDDADADDA CBEBDADDDA DADDB BCBEBADBA BCED DDDABBBBD BCCBA EA BEAEC CBA CBBAECB BBADA BBABC CDA BEBBABC CEEA ABD BAA CCBA CBBAA CBC ADA BDDE CED) 1)
;;(dendrogram '(EEEB BEEC CBEAEB BAEC CBEBDD DDDADADDA CBEBDADDDA DADDB BCBEBADBA BCED DDDABBBBD BCCBA EA BEAEC CBA CBBAECB BBADA BBABC CDA BEBBABC CEEA ABD BAA CCBA CBBAA CBC ADA BDDE CED) 2 :diss-fun (lambda* (a b) (structure-distance a b :root t)) :newick "root" :and-data t)

;;==================================================================
;;           SYSTEMIC ANALYSIS Derivative clustering
;;==================================================================
(defgeneric serial-pair (in))
(defmethod serial-pair ((str string))
  (loop for i from 1 to (1- (length str)) collect (subseq str (1- i) (1+ i))))

(defmethod serial-pair ((lst list))
  (loop for i from 1 to (1- (length lst)) collect (list (nth (1- i) lst) (nth i lst))))
;;------------------------------------------------------------------
(defun seq->dx (seq &optional (diss-fun '(structure-distance)))
  (loop for i in (serial-pair seq) collect (funcall diss-fun i)))

(defun mean-diss (seq &optional (diss-fun '(structure-distance)))
  (if (= 1 (length seq)) 0 (mean (loop for i in (all-pairs seq) collect (funcall diss-fun i)))))

(defmethod group-list ((lst list) (segmentation list) &optional mode)  
  "Segments a <lst> in successives sublists 
which lengths are successive values of the list <segmentation>.
 <mode> indicates if <list> is to be read in a circular way."
  (let ((list2 lst) (res nil))
    (catch 'gl
      (loop for segment in segmentation
	 while (or list2 (eq mode 'circular))
	 do (push (loop for i from 1 to segment
		     when (null list2)
		     do (ecase mode
			  (linear (push sublist res) (throw 'gl 0))
			  (circular (setf list2 lst)))
		     end
		     collect (pop list2) into sublist
		     finally (return sublist))
		  res)))
    (nreverse res)))
;;------------------------------------------------------------------
(defmethod gnuplot> ((lst list) &key mlt (w 1200) (h 600) (scale 1) (fontsize 15) (gnuplot *gnuplot*))
  ;; i defines the time length and it is only for gnuplot graphic convenience.
  ;; if som is a mlt then the x-axis is the timimg of events, else the interval is equidistant.
  (write-file (loop for i in
		   (if (mlt-p mlt)
		       (dx->x 0 (mapcar #'(lambda (x) (apply #'+ x)) (group-list (mapcar #'car (cdr (cadr (carte mlt)))) (loop for l in (mapcar #'string2list (mapcar #'string lst)) collect (length l)))))
		       (loop for a from 1 to (length lst) collect a))
		 for j in (seq->dx lst) for k in lst collect (list i (float j) k)) :name "derivative.dat" :path *N3-BACKUP-DIRECTORY*)
  (let ((dat (list
	      (format nil "set terminal png size ~S,~S font \"Courier, ~S\"" (round (* scale w)) (round (* scale h)) fontsize)
	      (format nil "set output '~Aderivative.png'" *N3-BACKUP-DIRECTORY*)
	      (format nil "unset xtics~&unset ytics~&unset border~&set datafile separator \" \"~&set yrange [0:]~&set bmargin 0~&set tmargin 10~&f(x)=~S" (mean-diss lst))    
	      (format nil "plot f(x) title '~A' with lines, \"~Aderivative.dat\" using 1:2:3 title '' with labels rotate by 90 left offset character 0,0 font \"monospace,~S\"" (if (mlt-p mlt) (name mlt) (if mlt mlt "")) *N3-BACKUP-DIRECTORY* fontsize))))
    (write-file dat :name "derivative.pl" :path *N3-BACKUP-DIRECTORY*))
  (UIOP::run-program (format nil "sh -c '~S ~S'" gnuplot (concatenate 'string *N3-BACKUP-DIRECTORY* "derivative.pl"))))

(defmethod open-graph ((self list) &key mlt (w 1200) (h 600) (scale 1) (fontsize 15) (gnuplot *gnuplot*) (display *display*)) 
  (gnuplot> self :mlt mlt :w w :h h :scale scale :fontsize fontsize :gnuplot gnuplot)
  (UIOP::run-program (format nil "sh -c '~S ~S'" display (format nil "~A/derivative.png" *N3-BACKUP-DIRECTORY*))))

;;(open-graph '(EEEB BEEC CBEAEB BAEC CBEBDD DDDADADDA CBEBDADDDA DADDB BCBEBADBA BCED DDDABBBBD BCCBA EA BEAEC CBA CBBAECB BBADA BBABC CDA BEBBABC CEEA ABD BAA CCBA CBBAA CBC ADA BDDE CED) :mlt <som>)
;;------------------------------------------------------------------
(defun part-s (seq)
  (let* ((al (loop for j in (seq->dx seq) for k in seq collect (list (float j) k)))
	 (md (mean-diss seq))
	 tmp r)
    (loop for i in al do
	 (cond ((null tmp) (push i tmp))
	       ((and (>= (car i) md) (>= (caar tmp) md)) (push i tmp))
	       ((and (< (car i) md) (< (caar tmp) md)) (push i tmp))
	       (t (push tmp r) (setf tmp (list i)))))
    (push tmp r)
    (loop for i in (reverse r) collect (reverse (mapcar #'cadr i)))))
;;------------------------------------------------------------------
;;(cadar (ordinate (mapcar #'reverse (history (flat-once (loop for i in '(EEEB BEEC CBEAEB BAEC CBEBDD DDDADADDA CBEBDADDDA DADDB BCBEBADBA BCED DDDABBBBD BCCBA EA BEAEC CBA CBBAECB BBADA BBABC CDA BEBBABC CEEA ABD BAA CCBA CBBAA CBC ADA BDDE CED) collect (serial-pair (string i)))))) '> :key 'car))

;;==================================================================
;;             SYSTEMIC ANALYSIS Developmental process
;;==================================================================
(defun get-match (head lw)
  (if head
      (loop for i in lw when (equalp (butlast i) head) collect i)
      lw))

(defmethod next-event-probability ((head list) (seq list) &key (result :verbose) remanence (compute #'rnd-weighted) opt)
  (declare (ignore remanence opt))
  (let* ((lw (get-match head (loop-wind seq (1+ (length head)))))
	 (rh (history lw))
	 (ord (ordinate (mapcar #'list (mapcar #'car rh) (normalize-sum (mapcar #'cadr rh))) #'> :key #'car)))
    (case result
      (:prob (mapcar #'reverse ord))
      (:verbose (loop for i in ord do
		     (format t "~@<~S => ~3I~_~,6f %~:>~%" (if (singleton (car i)) (caar i) (car i)) (* 100 (float (cadr i))))))
      (:eval (when lw
	       (multiple-value-bind (a b) (funcall compute ord)
		 (values
		  (if (singleton a) (car a) a) ;; the result itself 
		  ;(length rh)                 ;; number of potential candidat
		  b)))))))                     ;; probability of the selected candidat

;;------------------------------------------------------------------

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(make-instance 'neuron :name 'neuron)
(make-instance 'neuron :name 'ghost)
(defconstant MLT (make-instance 'mlt))
(format t "; MLT default functions:~&; DISTANCE-IN: ~S~&; DISTANCE-OUT: ~S~&; VOISINAGE: ~S~&; CARTE: ~S~&" (DISTANCE-IN MLT) (DISTANCE-OUT MLT) (VOISINAGE MLT) (CARTE MLT))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
