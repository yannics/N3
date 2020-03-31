;; NEUROMUSE3
;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                              DENDROGRAM ALGORITHM
;                                          13/20-10-12 rev/14-05-16

;------------------------------------------------------------------
;                                                             UTILS

;; allows to use dendrogram with a list of objects as event --> see analysis.lisp file.
(defvar *event* nil)

(defparameter *n-round* 8)
(defparameter +GA+ (make-instance 'neuron) "Ghost neuron for the center of gravity of a class A, in order to evaluate a distance with +GB+.")
(defparameter +GB+ (make-instance 'neuron) "Ghost neuron for the center of gravity of a class B, in order to evaluate a distance with +GA+.")

(defgeneric roundd (in n))
(defmethod roundd ((in number) (n integer))
  (float (/ (round (* in (expt 10 n))) (expt 10 n))))
(defmethod roundd ((in list) (n integer))
  (loop for i in in collect (roundd i n)))

(defun read-value-in (value)
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

(defstruct (node (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (node-label p))))) label child parent dist (inertia 0.0) data)
;; label[node] = -<distance-number>+(...) or <neuron-name>+ [leaf]
;; data[node] = <neuron-name> [leaf] or (list <som-name> <aggregation-number> (list <distance-function> &optional ...)) [root] or nil [node]
;; dist[node] is used only to built newick tree, the real distance is defined by (abs (read-value-in (node-label <node>))) [node] or 0.0 [leaf] (neuron)
;; inertia[node] = intra-class inertia from leaves of the node

(defgeneric root-p (self))
(defgeneric leaf-p (self))
(defgeneric get-root (self))
(defgeneric get-leaves (self &key trim loop))
(defgeneric tree>nw (self &key with-label as-root))
(defgeneric rec-format (self &optional stream))
(defgeneric cah-fanaux (self tree n-class &key trim))
(defgeneric dendrogram (self aggregation &key diss-fun newick with-label and-data) (:documentation "The node-data root records as a list: self aggregation diss-fun string-info"))

(defmethod root-p ((self node)) (when (null (node-parent self)) t))
(defmethod root-p ((self t)) nil)

(defmethod leaf-p ((self node)) (when (null (node-child self)) t))
(defmethod leaf-p ((self t)) nil)

(defmethod id ((self node)) self)

(defmethod get-root ((self node))
  (if (null (node-parent self)) self (get-root (id (node-parent self)))))

(defmethod get-leaves ((self node) &key trim loop)
  (print self)
  "Get the list of leaves of a given node.
- trim = nil: collect all leaves of the tree or the subtree (equivalent to prune) of a given node.
- trim = <distance/node>: remove nodes beyond a given distance or a given node and collect all leaves from the root."
  ;; leaves collector initialisation
  (when (null loop) (setf *memcache1* nil))
  (labels ((rec (node tr) (loop for i in (node-child node) collect (get-leaves (id i) :trim tr :loop t))))
    (if (or (node-p trim) (numberp trim)) 
	(if (leaf-p self)
	    (push self *memcache1*)
	    (if (> (* (expt 10 *n-round*) (abs (read-value-in (node-label self))))
		   (if (node-p trim)
		       (1- (* (expt 10 *n-round*) (abs (read-value-in (node-label trim)))))
		       trim))
		(rec self trim)
		(push self *memcache1*)))
	(get-leaves self :trim 1)))
  ;; output leaves list
  *memcache1*)
#|
#+sbcl
; in: DEFMETHOD GET-LEAVES (NODE)
;     (N3::REC N3::SELF N3::TRIM)
; ==>
;   N3::SELF
; 
; note: deleting unreachable code
; 
; note: deleting unreachable code
#+openmcl
|#

(defmethod tree>nw ((self node) &key with-label (as-root t))
  "Display as a newick structure the <self> node."
  (if as-root
      (let ((copy-node (make-node :label (node-label self) :child (node-child self))))
	  (tree>nw copy-node :with-label with-label :as-root nil))
      (if (leaf-p self)
	  (read-from-string (format nil "~A!~A" (node-data self) (node-dist self)))
	  (cons (loop for i in (node-child self) collect (tree>nw i :with-label with-label :as-root nil))
		(if (root-p self)
		    (when with-label (list (read-from-string (format nil "~A" (read-value-in (node-label self))))))
		    (list (read-from-string (format nil "~A!~A" (if with-label (read-value-in (node-label self)) "") (node-dist self)))))))))
  
(defun update-tree (it fn-diss)
  (setf *memcache2* nil)
  (loop for i in *tree* do (when (not (member i (car it) :test #'equalp)) (push i *memcache2*)))
  (let ((distl (loop for i in (car it) when (not (leaf-p i)) collect (abs (read-value-in (node-label i)))))
	(name (intern (format nil "~S" (cadr it)))))
    (if (and distl (not (loop for d in distl never (> d (abs (read-value-in (cadr it)))))))
	(let (nnode mnode)
	  (cond ((leaf-p (caar it)) (setf nnode (cadar it) mnode (caar it)))
		((leaf-p (cadar it)) (setf nnode (caar it) mnode (cadar it)))
		((> (abs (read-value-in (node-label (caar it)))) (abs (read-value-in (node-label (cadar it))))) (setf nnode (caar it) mnode (cadar it)))
		(t (setf nnode (cadar it) mnode (caar it))))
	  (setf (node-child nnode) (append (node-child nnode) (if (leaf-p mnode) (list mnode) (node-child mnode))))
	  (push nnode *memcache2*))
	(push (setf (symbol-value name) (make-node :label name :child (car it))) *memcache2*)))   
  (loop for n in (node-child (car *memcache2*)) do
       (setf (node-parent n) (car *memcache2*)
	     (node-dist n) (if (leaf-p n)
			       (round (* (expt 10 *n-round*) (abs (read-value-in (node-label (car *memcache2*))))))
			       (round (* (expt 10 *n-round*) (- (read-value-in (node-label n)) (read-value-in (node-label (car *memcache2*)))))))))
  (unless *event*
    (dolist (e *memcache2*)
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
  (if res
      (let ((gl (get-leaves (get-root self) :trim (1- (round (* (expt 10 *n-round*) (abs (read-value-in (node-label self)))))))))
	(if (loop for l in gl always (leaf-p l))
	    (append (reverse (butlast res)) (last res))
	    (let* ((sl (loop for i in gl unless (leaf-p i) collect i))
		   (wn (car (ordinate sl #'> :key #'(lambda (x) (abs (read-value-in (node-label x))))))))
	      (history (id wn)
		       (cons
			(list
			 gl
			 ;; get the minimum distance between the set of nodes and their common parent.
			 (- (abs (read-value-in (node-label self))) (abs (read-value-in (node-label wn))))
			 ;; intra-class inertia as sum of all intra-class inertia from sl (trim). 
			 (reduce #'+ (mapcar #'node-inertia sl))
			 )
			res)))))
      (history (get-root self) (cons (list (get-leaves (get-root self)) 0.0 0.0) nil))))

(defgeneric neuron>ind (self))
(defmethod neuron>ind ((self node)) (if (neuron-p (id (read-value-in self))) (read-from-string (format nil "~S+" (neuron>ind (id (read-value-in self))))) self))
(defmethod neuron>ind ((self neuron)) (+ 10 (ind self)))
(defmethod neuron>ind ((self t)) self)
(defgeneric neuron<ind (self n))
(defmethod neuron<ind ((self rna) (n node)) (if (integerp (read-value-in n)) (read-from-string (format nil "~S+" (neuron<ind self (read-value-in n)))) n))
(defmethod neuron<ind ((self rna) (n integer)) (id (nth (- n 10) (neurons-list self))))
(defmethod neuron<ind ((self rna) (n t)) n)

(defmethod save ((self node))
  (let ((path
	 (if *event*
	     (concatenate 'string *event* ".tree")
	     (format nil "~A~A/~A~A~A.tree" *N3-BACKUP-DIRECTORY* (car (node-data self)) (cadr (node-data self)) (node-label self) (epoch (id (read-from-string (car (node-data self)))))))))
    (unless *event* (ensure-directories-exist (format nil "~A~A/" *N3-BACKUP-DIRECTORY* (car (node-data self)))))
    (progn
      (with-open-file (stream path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	;-----------------------  
	(loop for n in (cons self (remove-duplicates (flatten (mapcar #'car (history self))) :test #'equalp))
	   do
	     (format stream "(DEFPARAMETER ~S (MAKE-NODE :LABEL (QUOTE ~S) :CHILD (QUOTE ~S) :PARENT (QUOTE ~S) :DIST (QUOTE ~S) :INERTIA (QUOTE ~S) :DATA ~S)) "
		     (neuron>ind (id (node-label n)))
		     (neuron>ind (id (node-label n)))
		     (loop for i in (node-child n) collect (neuron>ind (id i)))
		     (neuron>ind (id (node-parent n)))
		     (node-dist n)
		     (node-inertia n)
		     (if (and (listp (node-data n)) (not (null (node-data n)))) (cons 'list (node-data n)) (neuron>ind (id (node-data n))))))
	;-----------------------
	(format stream "(SETF *TREE* ~S)" self))
      (UIOP:run-program (format nil "sh -c '~S ~S ~S'" *UPDATE-SAVED-NET* path (if *event* 1 0))))))

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
      (format t ";---------------~%")
      (dotimes (item-number (length items))
	(format t "~45<~A: ~A ...~;... ~A~>~%" (+ item-number 1)
		(cadr (reverse (split-path (string (nth item-number items)))))
		(caddr (reverse (split-path (string (nth item-number items)))))
		))
      (format t "Load (Type any key to escape): ")
      (let ((val (read))) 
	(when (member val (loop for i from 1 to (length items) collect i))
	  (let* ((dir (concatenate 'string *N3-BACKUP-DIRECTORY* (subseq (string (nth (- val 1) items)) 2 (- (length (string (nth (- val 1) items))) 4)) "tree"))
		 (rna (read-from-string (caddr (reverse (split-path dir))))))
	    (if (som-p (id rna))
		(progn
		  (UIOP:run-program (format nil "sh -c 'cd ~A; cp ~A .tmp'" (directory-namestring dir) dir))
		  (loop for i in (neurons-list (id rna))
		     do
		       (UIOP:run-program (format nil "sh -c 'cd ~A; cat .tmp | sed \"s/ ~S+/ ~S+/g; s/(~S+/(~S+/g; s/:DATA ~S)/:DATA (QUOTE ~S))/g\" > .foo; mv .foo .tmp'" (directory-namestring dir) (neuron>ind (id i)) (neuron<ind (id rna) (id i)) (neuron>ind (id i)) (neuron<ind (id rna) (id i)) (neuron>ind (id i)) (neuron<ind (id rna) (id i)))))
		  (load (concatenate 'string (directory-namestring dir) ".tmp"))
		  (format t "~45<TREE[~A] ...~;... loaded ...~>~%" (tff (string (caddr (split-path (string (nth (1- val) items)))))))
		  (UIOP:run-program (format nil "sh -c 'cd ~A; rm .tmp'" (directory-namestring dir))))
		(warn "This tree has been built from the neural network ~S. The latter should be loaded first ..." rna))))))))

;------------------------------------------------------------------
;                                                        DENDROGRAM

(defun mk-diss (pair fn-diss &key ward)
  (let ((lst1 (get-leaves (car pair)))
	(lst2 (get-leaves (cadr pair))))
    (if (and ward (null *event*))
	(progn
	  (gravity-center (mapcar #'node-data lst1) +GA+)
	  (gravity-center (mapcar #'node-data lst2) +GB+)
	  (let ((dist (roundd (* (/ (* (length lst1) (length lst2)) (+ (length lst1) (length lst2))) (expt (funcall fn-diss +GA+ +GB+) 2)) *n-round*)))
	    (if (zerop dist) (error "Increase the value of *n-round* to avoid distance equal to zero.") dist)))
    (let (r)
      (loop for i in (loop for az in lst1 collect (read-value-in az))
	 do
	   (when (not (numberp i))
	     (loop for j in (loop for az in lst2 collect (read-value-in az))
		do
		  (when (and (not (numberp j)) (not (eq i j)))
		    (push (funcall fn-diss
				   (if *event* (format nil "~S" i) (id i))
				   (if *event* (format nil "~S" j) (id j))) r)))))
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
		     (3 (unless *event* (cons (mk-diss i fn-diss :ward t) i))))))
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

(defmethod dendrogram ((self som) (aggregation integer) &key (diss-fun #'euclidean) (newick t) with-label and-data)
  (setf
   *event* nil
   *tree* '())
  (dolist (e (neurons-list self)) (push (setf (symbol-value (intern (format nil "~S+" e))) (make-node :label (read-from-string (format nil "~A+" e)) :data (id e))) *tree*))
  (dendro *tree* aggregation diss-fun)
  (setf (node-data (car *tree*)) (list (format nil "~S" self) aggregation
				       (let ((mvl (multiple-value-list (function-lambda-expression diss-fun))))
					 (cond
					   ((listp (car (last mvl))) diss-fun)				
					   (t (read-from-string (format nil "#'~S" (car (last mvl)))))))
				       (format nil "~S~S~S" aggregation (car *tree*) (epoch self))))
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
    (when and-data (>data-file (format nil "~A~A/~A~A~A/~A.dat" *N3-BACKUP-DIRECTORY* (name self) aggregation (car *tree*) (epoch self) (name self)) (mapcar #'(lambda (x) (cons (length (car x)) (cdr x))) (history (car *tree*))))))
  (setf *tree* (car *tree*)))
  
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; ... update MLT with CAH ...
; (update-fanaux <mlt> (cah-fanaux <mlt> (dendrogram <mlt> 3 :newick nil) <n-class/(length_(fanaux-list_<mlt>))>))

;------------------------------------------------------------------


