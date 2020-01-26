;; NEUROMUSE3
;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                              DENDROGRAM ALGORITHM
;                                          13/20-10-12 rev/14-05-16

;------------------------------------------------------------------
;                                                             UTILS

(defparameter *n-round* 8)
(defparameter +GA+ (make-instance 'neuron) "Ghost neuron for the center of gravity of a class A, in order to evaluate a distance with +GB+.")
(defparameter +GB+ (make-instance 'neuron) "Ghost neuron for the center of gravity of a class B, in order to evaluate a distance with +GA+.")

(defgeneric roundd (in n))
(defmethod roundd ((in number) (n integer))
  (float (/ (round (* in (expt 10 n))) (expt 10 n))))
(defmethod roundd ((in list) (n integer))
  (loop for i in in collect (roundd i n)))

(defun mat-trans (lst)
  (apply #'mapcar #'list lst))

(defun read-value-by (value)
  (read-from-string (remove-if #'(lambda (x) (equalp x #\+)) (prin1-to-string value))))

;;-----------------------------------------------------------------
;;                                                          LINKAGE

;; single linkage 
(defun mini (lst) ;; lst --> list of dissimilarity between 2 classes
  (loop for i in lst
     minimize (if (numberp i) i (car i))))

;; complete linkage 
(defun maxi (lst) ;; lst --> list of dissimilarity between 2 classes
  (loop for i in lst
     maximize (if (numberp i) i (car i))))

;; Ward's method
(defgeneric gravity-center (lst ghost-neuron)
  (:documentation "The slots xpos and output of the ghost-neuron are the centers of gravity of the set of neurons of lst."))

(defmethod gravity-center ((lst list) (ghost-neuron neuron)) ;; lst --> list of neurons
  (setf (xpos ghost-neuron) (mapcar #'mean (mat-trans (loop for i in lst collect (xpos (id i)))))
	(output ghost-neuron) (mapcar #'mean (mat-trans (loop for j in lst collect (output (id j)))))
	(net ghost-neuron) (net (id (car lst)))))

;------------------------------------------------------------------
;                                                              TREE

(defvar *tree* nil)
(defvar *memcache1* nil)
(defvar *memcache2* nil)

(Defstruct (node (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (node-label p))))) label child parent dist inertia data)
;; label[node] = -<distance-number>+(...) or <neuron-name>+ [leaf]
;; data[node] = <neuron-name> [leaf] or (list <som-name> <aggregation-number> (list <distance-function> &optional ...)) [root] or nil [node]
;; dist[node] is used only to built newick tree, the real distance is defined by (abs (read-value-by (node-label <node>))) [node] or 0.0 [leaf] (neuron)
;; inertia[node] = intra-class inertia from leaves of the node

(defgeneric root-p (self))
(defgeneric leaf-p (self))
(defgeneric get-root (self))
(defgeneric get-leaves (self &key trim loop))
(defgeneric tree>nw (self &key with-label as-root))
(defgeneric rec-format (self &optional stream))
(defgeneric cah-fanaux (self tree n-class &key trim))
(defgeneric dendrogram (self aggregation &key diss-fun newick with-label with-data))

(defmethod root-p ((self node)) (when (null (node-parent self)) t))
(defmethod root-p ((self t)) nil)

(defmethod leaf-p ((self node)) (when (null (node-child self)) t))
(defmethod leaf-p ((self t)) nil)

(defmethod id ((self node)) self)

(defmethod get-root ((self node))
  (if (null (node-parent self)) self (get-root (id (node-parent self)))))

(defmethod get-leaves ((self node) &key trim loop)
  "Get the list of leaves of a given node.
- trim = nil: collect all leaves of the tree or the subtree (equivalent to prune) of a given node.
- trim = <distance/node>: remove nodes beyond a given distance or a given node and collect all leaves from the root."
  ;; leaves collector initialisation
  (when (null loop) (setf *memcache1* nil))
  (labels ((rec (node tr) (loop for i in (node-child node) collect (get-leaves (id i) :trim tr :loop t))))
    (if (null trim)
	(let ((copy-node (make-node :label (node-label self) :child (node-child self) :data (node-data self))))
	  (get-leaves copy-node :trim 1))
	(if (leaf-p self)
	    (push self *memcache1*)
	    (if trim
		(if (> (* (expt 10 *n-round*) (abs (read-value-by (node-label self)))) (cond ((node-p trim) (1- (* (expt 10 *n-round*) (abs (read-value-by (node-label trim)))))) ((numberp trim) trim) (t (error "The key trim has to be a number or a node."))))
		    (rec self trim)
		    (push self *memcache1*))
		(rec self trim)))))
  ;; output leaves list
  *memcache1*)

(defmethod tree>nw ((self node) &key with-label (as-root t))
  "Display as a newick structure the <self> node."
  (if as-root
      (let ((copy-node (make-node :label (node-label self) :child (node-child self))))
	  (tree>nw copy-node :with-label with-label :as-root nil))
      (if (leaf-p self)
	  (read-from-string (format nil "~A!~A" (node-data self) (node-dist self)))
	  (cons (loop for i in (node-child self) collect (tree>nw i :with-label with-label :as-root nil))
		(if (root-p self)
		    (when with-label (list (read-from-string (format nil "~A" (read-value-by (node-label self))))))
		    (list (read-from-string (format nil "~A!~A" (if with-label (read-value-by (node-label self)) "") (node-dist self)))))))))
  
(defun update-tree (it fn-diss)
  (setf *memcache2* nil)
  (loop for i in *tree* do (when (not (member i (car it) :test #'equalp)) (push i *memcache2*)))
  (let ((distl (loop for i in (car it) when (not (leaf-p i)) collect (abs (read-value-by (node-label i)))))
	(name (intern (format nil "~S" (cadr it)))))
    (if (and distl (not (loop for d in distl never (> d (abs (read-value-by (cadr it)))))))
	(let (nnode mnode)
	  (cond ((leaf-p (caar it)) (setf nnode (cadar it) mnode (caar it)))
		((leaf-p (cadar it)) (setf nnode (caar it) mnode (cadar it)))
		((> (abs (read-value-by (node-label (caar it)))) (abs (read-value-by (node-label (cadar it))))) (setf nnode (caar it) mnode (cadar it)))
		(t (setf nnode (cadar it) mnode (caar it))))
	  (setf (node-child nnode) (append (node-child nnode) (if (leaf-p mnode) (list mnode) (node-child mnode))))
	  (push nnode *memcache2*))
	(push (setf (symbol-value name) (make-node :label name :child (car it))) *memcache2*)))   
  (loop for n in (node-child (car *memcache2*)) do
       (setf (node-parent n) (car *memcache2*)
	     (node-dist n) (if (leaf-p n)
			       (round (* (expt 10 *n-round*) (abs (read-value-by (node-label (car *memcache2*))))))
			       (round (* (expt 10 *n-round*) (- (read-value-by (node-label n)) (read-value-by (node-label (car *memcache2*)))))))))
  (dolist (e *memcache2*)
		  (when (null (node-inertia e))
		    (setf (node-inertia e)
			  (let* ((al (mapcar #'node-data (get-leaves e))))
			    (gravity-center al +GA+)
			    (/ (loop for l in al sum
				    (expt
				     (funcall fn-diss +GA+ l) 2))
			       (length al))))))
  (setf *tree* *memcache2*))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defmethod history ((self node) &optional res)
  (let ((gl (get-leaves (get-root self) :trim (1- (round (* (expt 10 *n-round*) (abs (read-value-by (node-label self)))))))))
    (if (loop for l in gl always (leaf-p l))
	(reverse res)
	(let* ((sl (loop for i in gl unless (leaf-p i) collect i))
	       (wn (car (ordinate sl #'> :key #'(lambda (x) (abs (read-value-by (node-label x))))))))
	  (history (id wn)
		   (push
		    (list
		     gl
		     (if (leaf-p self) 0.0
			 ;; get the minimum distance between the set of nodes and their common parent.
			 (- (abs (read-value-by (node-label self))) (abs (read-value-by (node-label wn)))))
		     ;; intra-class inertia as sum of all intra-class inertia from sl (trim). 
		     (reduce #'+ (mapcar #'node-inertia sl))
		     )
		    res))))))

(defmethod save ((self node))
  (let* ((scriptpath (format nil "~Abin/update-saved-net" *NEUROMUSE3-DIRECTORY*))
	 (path (format nil "~A~A/~A~A~A.tree" *N3-BACKUP-DIRECTORY* (car (node-data self)) (cadr (node-data self)) (node-label self) (epoch (id (car (node-data self)))))))
    (ensure-directories-exist (format nil "~A~A/" *N3-BACKUP-DIRECTORY* (car (node-data self))))
    (unless (mlt-p (id (car (node-data self)))) (save (id (car (node-data self)))))
    (progn
      (with-open-file (stream path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream "(IN-PACKAGE :N3) ")
	;-----------------------
	(loop for n in (remove-duplicates (flatten (mapcar #'car (history self))) :test #'equalp)
	   do
	     (format stream "(UNLESS (NODE-P ~S) (DEFPARAMETER ~S NIL)) " (node-label n) (node-label n))
	     (format stream "(SETF ~S (MAKE-NODE :LABEL (QUOTE ~S) :CHILD (QUOTE ~S) :PARENT (QUOTE ~S) :DIST (QUOTE ~S) :INERTIA (QUOTE ~S) :DATA ~S)) "
		     (node-label n)
		     (node-label n)
		     (node-child n)
		     (node-parent n)
		     (node-dist n)
		     (node-inertia n)
		     (cond ((and (listp (node-data n)) (functionp (nth 2 (node-data n))))
			    (let ((mvl (multiple-value-list (function-lambda-expression (nth 2 (node-data n))))))
			      (cond
				((and (eq 'LAMBDA (caar mvl)) (not (listp (car (last mvl)))))
				 (read-from-string (format nil "(QUOTE (~S ~S #'~S ~S))" (nth 0 (node-data n)) (nth 1 (node-data n)) (car (last mvl)) (nth 3 (node-data n)))))
				((and (eq 'LAMBDA (caar mvl)) (eq 'LAMBDA (caar (last mvl))))
				 (read-from-string (format nil "(QUOTE (~S ~S #'~S ~S))" (nth 0 (node-data n)) (nth 1 (node-data n)) (car mvl) (nth 3 (node-data n)))))
				(t
				 (read-from-string (format nil "(QUOTE (~S ~S #'~S ~S))" (nth 0 (node-data n)) (nth 1 (node-data n)) (car (last mvl)) (nth 3 (node-data n))))))))
			   ((null (node-data n))
			    (read-from-string (format nil "NIL")))
			   (t
			    (read-from-string (format nil "(QUOTE ~S)" (node-data n)))))))
	;-----------------------
	(format stream "(SETF *TREE* ~S) " self)
	(format stream "(WHEN (NOT (MEMBER (BOUND-TEST (QUOTE ~S)) *AVAILABLE-SOM*)) (LET ((FILE (FORMAT NIL \"~~A~S.som\" *N3-BACKUP-DIRECTORY*))) (IF (OPEN FILE :IF-DOES-NOT-EXIST NIL) (LOAD-NEURAL-NETWORK FILE) (WARN \"This tree has been built from the neural network ~S. The latter should be loaded ...\"))))" (car (node-data self)) (car (node-data self)) (car (node-data self))))
      (UIOP:run-program (format nil "sh -c '~S ~S'" scriptpath path)))))

(defun split-path (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\/ string :start i)
     collect (subseq string i j)
     while j))

(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
	(cons string r))))

(defun tff (str)
  (concatenate 'string "-" (car (split-1 (cadr (split-1 str "-")) "+")) "+"))

(defun tree-menu ()
  (let* ((stdout (format nil "~a" (UIOP:run-program (format nil "sh -c 'cd ~S; find . -type f -name \"*.tree\"'" *N3-BACKUP-DIRECTORY*) :output :string)))
	 (items (string-to-list stdout)))
    (when items
      (dotimes (item-number (length items))
	(format t "~45<~A: ~A ...~;... ~A~>~%" (+ item-number 1)
		(cadr (reverse (split-path (string (nth item-number items)))))
		(caddr (reverse (split-path (string (nth item-number items)))))
		))
      (format t "Load (Type any key to escape): ")
      (let ((val (read))) 
	(when (member val (loop for i from 1 to (length items) collect i))
	  (load (concatenate 'string *N3-BACKUP-DIRECTORY* (subseq (string (nth (- val 1) items)) 2 (- (length (string (nth (- val 1) items))) 4)) "tree"))
	  (format t "~45<TREE[~A] ...~;... loaded ...~>~%" (tff (string (caddr (split-path (string (nth (1- val) items))))))))))))

;------------------------------------------------------------------
;                                                        DENDROGRAM

(defun mk-diss (pair fn-diss &key ward)
  (let ((lst1 (get-leaves (car pair)))
	(lst2 (get-leaves (cadr pair))))
    (if ward
	(progn
	  (gravity-center (mapcar #'node-data lst1) +GA+)
	  (gravity-center (mapcar #'node-data lst2) +GB+)
	  (let ((dist (roundd (* (/ (* (length lst1) (length lst2)) (+ (length lst1) (length lst2))) (expt (funcall fn-diss +GA+ +GB+) 2)) *n-round*)))
	    (if (zerop dist) (error "Increase the value of *n-round* to avoid distance equal to zero.") dist)))
    (let (r)
      (loop for i in (loop for az in lst1 collect (read-value-by az))
	 do
	   (when (not (numberp i))
	     (loop for j in (loop for az in lst2 collect (read-value-by az))
		do
		  (when (and (not (numberp j)) (not (eq i j)))
		    (push (funcall fn-diss (id i) (id j)) r)))))
      (roundd r *n-round*)))))

(defun rec-if (val)
  "Name or rename val."
  (let ((val1 (read-from-string (format nil "~S+" val))))
    (if (ignore-errors (member (symbol-value val1) *tree*))
	(rec-if val1) val1)))

(defun aggregate (lst a fn-diss)
  "a = aggregation method:
if a = 1 --> single linkage
   a = 2 --> complete linkage
   a = 3 --> Ward method"
  (let* ((lst (all-pairs lst))
         (res (loop for i in lst
		 collect
		   (case a
		     (1 (cons (mini (mk-diss i fn-diss)) i))
		     (2 (cons (maxi (mk-diss i fn-diss)) i))
		     (3 (cons (mk-diss i fn-diss :ward t) i)))))
	 (result (reverse (assoc (loop for i in res minimize (car i)) res))))
    (list (butlast result)
	  (rec-if (* -1 (car (last result)))))))

(defun dendro (lst a fn-diss) 
  (let ((res (update-tree (aggregate lst a fn-diss) fn-diss)))
    (if (= 1 (length res)) res
	(dendro res a fn-diss))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defmethod cah-fanaux ((self som) (tree node) (n-class integer) &key trim)
  "Built a fanaux list from a given number of classes according the CAH. 
For now tree has to be the node root."
  (let ((nodes (car (assoc n-class (history tree) :key #'length))))
    (if trim nodes
	(loop for i in nodes collect
	     (let ((gl (mapcar #'node-data (get-leaves i))))
	       (gravity-center gl +GA+)
	       (car (last (car (nearest self +GA+ :n-list gl :diss-fun (caddr (node-data tree)))))))))))

(defmethod dendrogram ((self som) (aggregation integer) &key (diss-fun #'euclidean) (newick t) with-label with-data)
  (setf *tree* '())
  (dolist (e (neurons-list self)) (push (setf (symbol-value (intern (format nil "~S+" e))) (make-node :label (read-from-string (format nil "~A+" e)) :data (id e))) *tree*))
  (dendro *tree* aggregation diss-fun)
  (setf (node-data (car *tree*)) (list (name self) aggregation diss-fun (format nil "~S~S~S" aggregation (car *tree*) (epoch self))))
  (when newick
    (save (car *tree*))
    (with-open-file (stream (make-pathname :directory (pathname-directory *N3-BACKUP-DIRECTORY*)
					   :name ".tmp"
					   :type "nw")
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "~S" (list (tree>nw (car *tree*) :with-label with-label))))
    (UIOP:run-program (format nil "sh -c '~S ~S ~S'" (format nil "~Abin/raw2nw" *NEUROMUSE3-DIRECTORY*) (format nil "~A.tmp.nw" *N3-BACKUP-DIRECTORY*) (format nil "~A~A/~A~A~A/~A.nw" *N3-BACKUP-DIRECTORY* (name self) aggregation (car *tree*) (epoch self) (name self))))
    (when with-data (>data-file (format nil "~A~A/~A~A~A/~A.dat" *N3-BACKUP-DIRECTORY* (name self) aggregation (car *tree*) (epoch self) (name self)) (mapcar #'(lambda (x) (cons (length (car x)) (cdr x))) (history (car *tree*))))))
  (setf *tree* (car *tree*)))
  
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; ... update MLT with CAH ...
; (update-fanaux <mlt> (cah-fanaux <mlt> (dendrogram <mlt> 3 :newick nil) <n-class/(length_(fanaux-list_<mlt>))>))

;------------------------------------------------------------------
