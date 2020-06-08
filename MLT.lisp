;; NEUROMUSE3
;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                            MLT (long term memory)      

;; define class MLT
(defclass MLT (SOM)
  ((net
    :initform nil :initarg :net :accessor net) ;; (AREA name which the MLT belongs)
   (fanaux-list
    :initform '() :initarg :fanaux-list :accessor fanaux-list :type list)
   (cover-value
    :initform 3 :initarg :cover-value :accessor cover-value)
   (mct
    :initform '() :initarg :mct :accessor mct :type list)
   (onset
    :initform (make-hash-table :test #'equalp) :initarg :onset :accessor onset)
   (fine
    :initform (make-hash-table :test #'equalp) :initarg :fine :accessor fine)
   (trns
    :initform (make-hash-table :test #'equalp) :initarg :trns :accessor trns)
   (arcs
    :initform (make-hash-table :test #'equalp) :initarg :arcs :accessor arcs)
   (mem-cache
    :initform nil :initarg :mem-cache :accessor mem-cache)))

(defgeneric mlt-p (self))
(defmethod mlt-p ((self mlt)) t)
(defmethod mlt-p ((self t)) nil)
(defmethod id ((self mlt)) self)

;------------------------------------------------------------------
;                                                UPDATE FANAUX-LIST     

(defgeneric nearest (self n &key n-list d-list diss-fun))
(defgeneric surjection (self new-fanaux-list &optional old-fanaux-list))
(defgeneric update-indice (self edge-list new-fanaux-list surjection &optional sp))
(defgeneric update-fanaux (self new-fanaux-list))

(defun ordinate (sequence predicate &key key)
  (let ((seq (copy-tree sequence)))
    (sort seq predicate :key key)))

;; http://stackoverflow.com/questions/6059317
(defun substitute-nth (val n list)
  (loop for i from 0 for j in list collect (if (= i n) val j)))

(defgeneric set-fanaux-list (self new-fanaux-list))
(defmethod set-fanaux-list ((self mlt) (nfl list))
  (if (net self)
      (let* ((area (id (net self)))
		 (som-index (position self (loop for i in (soms-list area) collect (id i)) :test #'equalp)))
	    (setf (fanaux-list self) nfl
		  (fanaux-length area) (substitute-nth (length nfl) som-index (fanaux-length area))))
      (setf (fanaux-list self) nfl)))

(defun node= (e1 e2 &key arcs)
  "Key arcs possible values are:
nil = check if e1 and e2 are in common one node;
11 = check if first e1 and first e2 are equal;
12 = check if first e1 and second e2 are equal;
21 = check if second e1 and first e2 are equal;
22 = check if second e1 and second e2 are equal."
  (cond ((null arcs) (or (equalp (car e1) (car e2)) (equalp (car e1) (cadr e2)) (equalp (cadr e1) (car e2)) (equalp (cadr e1) (cadr e2))))
	((= 11 arcs) (equalp (car e1) (car e2)))
	((= 12 arcs) (equalp (car e1) (cadr e2)))
	((= 21 arcs) (equalp (cadr e1) (car e2)))
	((= 22 arcs) (equalp (cadr e1) (cadr e2)))
	(t (warn "~A" (documentation 'node= 'function)))))

(defun rem-n (n lst)
  (if (null n)
      lst
      (if (listp n)
	  (rem-n (cdr n) (rem-n (car n) lst))
	  (loop for i in lst unless (equalp (id n) (ignore-errors (id i))) collect i))))

;; from http://www.lee-mac.com/insertnth.html
(defun insertnth (x n l)
  "This function inserts an item 'x' at the n(th) position in a list 'l'."
  (cond ((null l) nil)
	((< 0  n) (cons (car l) (insertnth x (1- n) (cdr l))))
	((cons x l))))

(defun flat-once (lst)
  (let (r) (loop for i in lst do
		(if (listp i) (dolist (e i r) (push e r)) (push nil r)))
       (reverse r)))

(defmethod nearest ((self mlt) (n neuron) &key n-list d-list diss-fun)
    (let ((tmp (ordinate (mapcar #'(lambda (x)
				     (list
				      (if (stringp diss-fun)
					  (eval (cons (read-from-string diss-fun) (list n (id x))))
					  (funcall (if diss-fun diss-fun (distance-in self)) n (id x)))  
				      x)) n-list) #'< :key #'car))) 
      (if d-list tmp
	  (loop for i in tmp until (> (car i) (caar tmp)) collect (insertnth n 1 i)))))

(defmethod surjection ((self mlt) (new-fanaux-list list) &optional old-fanaux-list)
  (let* ((newl (loop for i in new-fanaux-list collect (id i)))
	 (oldl (loop for i in (if old-fanaux-list old-fanaux-list (fanaux-list self)) collect (id i)))
	 (nlst (loop for f in newl collect (nearest self f :n-list oldl)))
	 (olst (loop for f in oldl collect (nearest self f :n-list newl)))
	 (tmp (mapcar #'append olst
		      (loop for i in oldl
			 collect
			   (loop for d in (flat-once nlst) when (node= (list i t) (cdr d)) collect d))))
	 (res (loop for i in oldl for j in tmp collect (remove-duplicates (mapcar #'(lambda (x) (rem-n i x)) j) :test #'equalp))))
    (labels ((get-fanal-alst (col ldist)
	       (loop for i in ldist while (<= (car i) (caar (ordinate col #'> :key #'car))) collect (cadr i))))
      (loop for i in res for j in oldl collect (get-fanal-alst i (nearest self j :n-list newl :d-list t))))))

(defmethod update-indice ((self mlt) (edge-list list) (new-fanaux-list list) (surjection list) &optional sp)
  (declare (ignore sp))
  (loop for i in edge-list collect (position (nth (random (length (nth i surjection))) (nth i surjection)) (loop for f in new-fanaux-list collect (id f)) :test #'equalp)))

(defun update-ht (table lst sr)
  (let ((mh (gethash lst table)))
    (setf (gethash lst table) (if mh (+ sr mh) sr))))

(defun replace-a (new n lst)
  (mapcar #'(lambda (a) (if (= (setq n (1- n)) -1) new a)) lst))

(defmethod update-fanaux ((self mlt) (new-fanaux-list list))
  (let ((nfl new-fanaux-list)
	(ct (get-universal-time)))
    ;; check valid neurons list ...
    (when (loop for x in nfl always (equalp self (id (net (id x)))))
      (if (fanaux-list self)
	  
	  (let ((sur (surjection self nfl))
		
		;; update hash tables mlt ...
		; get key and value in list
		(kav-trns (loop for key being the hash-keys of (trns self) collect (list key (gethash key (trns self)))))
		(kav-arcs (loop for key being the hash-keys of (arcs self) collect (list key (gethash key (arcs self))))))
	    ; update trns 
	    (clrhash (trns self))
	    (loop for i in kav-trns collect (update-ht (trns self) (update-indice self (car i) nfl sur) (cadr i)))
	    ; update arcs
	    (clrhash (arcs self))
	    (loop for i in kav-arcs collect (update-ht (arcs self) (update-indice self (car i) nfl sur) (cadr i)))
	    ; update mct
	    (setf (mct self) (update-indice self (mct self) nfl sur))
	    ; set date-report
	    (setf (gethash ct (date-report self)) (format nil "---> #<EPOCH ~S> ~~%---> #<NFL ~a> ~~%---> #<OFL ~a>" (epoch self) nfl (fanaux-list self)))
	    ; update fanaux-list
	    (set-fanaux-list self nfl)
	    
	    ;; update hash table area ...
	    (when (net self)
	      ; get som index and key and value in list				
	      (let* ((area (id (net self)))
		     (som-index (position self (loop for i in (soms-list area) collect (id i)) :test #'equalp))
		     (kav (loop for key being the hash-keys of (arcs area) collect (list key (gethash key (arcs area))))))
		; update arcs area
		(clrhash (arcs area))
		(setf (fanaux-length area) (replace-a (length nfl) som-index (fanaux-length area)))
		(loop for i in kav collect (update-ht (arcs area) (update-indice area (car i) nfl sur som-index) (cadr i)))	
		(setf (current-clique area) (loop for i in (current-clique area) collect (if (and (integerp i) (= i som-index)) (position (nth (random (length (nth i sur))) (nth i sur)) (loop for f in nfl collect (id f)) :test #'equalp) i))
		      (gethash ct (date-report area)) (format nil "#<MLT ~a> ~S" self ct)))))
	  
	  ;; reset all hash table and set new-fanaux-list ...
	  (progn
	    (clrhash (trns self))
	    (clrhash (arcs self))
	    (setf (mct self) '()
		  (fanaux-list self) nfl
		  (gethash ct (date-report self)) (format nil "---> #<EPOCH ~S> ~~%---> #<NFL ~a> ~~%---> #<OFL NIL>" (epoch self) nfl))))))
  (values))

(defmethod update-fanaux ((self mlt) (n-fanaux null)))

;------------------------------------------------------------------
;                                                    INITIALISATION              

(defun list! (x) (if (listp x) x (list x)))
(defclass ds () ; to manage and retrieve initial data values
  ((dt :initform nil :initarg :dt :accessor dt)))

(defmethod print-object ((self ds) stream)
  (format stream "#<DS~{ ~S~}>" (list! (dt self))))

(defgeneric ds-p (self)
  (:method ((self ds)) t)
  (:method ((self t)) nil))

(defmethod init-som :after ((self mlt) (nbre-input integer) (nbre-neurons integer) &key carte topology field)
  (declare (ignore nbre-input nbre-neurons carte topology field))
  (setf (gethash (1+ (get-universal-time)) (date-report self)) (make-instance 'ds :dt :bypass))
  (values self))

(defvar *available-som* '())
 
(defun create-mlt (name n-input n-neurons &key carte topology field n-fanaux)
  (push (init-som (make-instance 'mlt :name name) n-input n-neurons :carte carte :topology topology :field field) *available-som*)
  (eval (list 'defvar name '(symbol-value name)))
  (let ((mlt (car *available-som*)))
    (setf (input mlt) (make-list n-input :initial-element 0)
	  (neuron-gagnant mlt) (winner mlt))
    (when n-fanaux (update-fanaux mlt n-fanaux))
    mlt))

;------------------------------------------------------------------
;                                                   SEARCH-SPACE-IN

;; Inspired by M. Laurson
;; PATCHWORK: A Visual Programming Language and some Musical Applications.
;; Studia musica no.6, doctoral dissertation, Sibelius Academy, Helsinki, 1996.
;; chapter 5.1 PWCONSTRAINTS

(defgeneric search-space-in (self trn-sp))
(defgeneric node-match (car-node cadr-node htal))
(defgeneric test-trn (self trn))
(defgeneric tournoi-p (trn mlt ht) (:documentation "trn = list of fanaux indices; ht = keyword (:trns :arcs :trns? :arcs?)"))

(defun ar-ser (n &optional r)
  (dotimes (i n (reverse r)) (push i r)))

(defun cart (l1 l2)
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (append (list! x) (list y))) l2)) l1))

(defun reccomb (l1 l2 &optional (n 1))
  (unless (and (null l1) (null l2))
    (if (= (length l2) n) (flat-once (cart l1 (nth (1- n) l2)))
	(reccomb (flat-once (cart l1 (nth (1- n) l2))) l2 (1+ n)))))

(defmethod node-match ((car-node integer) (cadr-node integer) (htal list))
  (loop for i in htal when (equalp (list car-node cadr-node) i) collect i))

(defmethod node-match ((car-node integer) (cadr-node list) (htal list))
  (let ((nodes (loop for n in cadr-node collect (list car-node n))))
    (loop for i in htal when (member i nodes :test #'equalp) collect i)))

(defmethod node-match ((car-node list) (cadr-node integer) (htal list))
  (let ((nodes (loop for n in car-node collect (list n cadr-node))))
    (loop for i in htal when (member i nodes :test #'equalp) collect i)))

(defmethod node-match ((car-node integer) (cadr-node t) (htal list))
  (loop for i in htal when (eq car-node (car i)) collect i))

(defmethod node-match ((car-node t) (cadr-node integer) (htal list))
  (loop for i in htal when (eq cadr-node (cadr i)) collect i))

(defmethod node-match ((car-node list) (cadr-node list) (htal list))
  (let ((nodes (flat-once (loop for can in car-node collect (loop for cdn in cadr-node collect (list can cdn)))))) 
    (loop for i in htal when (member i nodes :test #'equalp) collect i)))

(defmethod node-match ((car-node t) (cadr-node list) (htal list))
  (flat-once (loop for i in cadr-node collect (node-match car-node i htal))))

(defmethod node-match ((car-node list) (cadr-node t) (htal list))
  (flat-once (loop for i in car-node collect (node-match i cadr-node htal))))

(defmethod node-match ((car-node t) (cadr-node t) (htal list))
  htal)

(defun lst>trn (lst)
  (when (loop for i in (butlast lst) for p from 1 always (eq (cadr i) (car (nth p lst)))) 
    (append (mapcar #'car lst) (list (cadar (last lst))))))

(defun trn>lst (trn)
  (loop for i in (butlast trn) for p from 1 collect (list i (nth p trn))))

(defun trn-match (trn htal)
  (let ((l (loop for i in (trn>lst trn) collect (node-match (car i) (cadr i) htal))))
    (if (member nil l) nil l)))

(defun get-arc-from-tournoi (lst &optional r) 
  (if (= 1 (length lst)) r
      (get-arc-from-tournoi (cdr lst) (append r (loop for i in (cdr lst) collect (list (car lst) i))))))

(defmethod test-trn ((self mlt) (trn list))
  (and (loop for i in trn always (or (eq '? i) (and (integerp i) (>= i 0) (< i (length (fanaux-list self)))))) (not (loop for i in trn always (eq '? i)))))

(defun ht (ht &optional (key :k))
  (case key
    (:k (let (r) (maphash (lambda (k v) (declare (ignore v)) (push k r)) ht) r))
    (:al (let (r) (maphash (lambda (k v) (push (cons k v) r)) ht) r))
    (:p (format t "~&~s~%" ht) (maphash (lambda (k v) (format t "~@<~S~20T~3I~_~S~:>~%" k v)) ht))))

(defmethod tournoi-p ((trn list) (self mlt) (ht symbol))
  (when (test-trn self trn)
    (case ht
      (:arcs (loop for i in (get-arc-from-tournoi trn) always (member i (ht (arcs self) :k) :test #'equalp)))
      (:arcs? (loop for i in (loop for i in (get-arc-from-tournoi trn) unless (member '? i) collect i) always (member i (ht (arcs self) :k) :test #'equalp)))
      (:trns (cond ((> (length trn) (cover-value self))
		    (loop for i in (loop-wind trn (cover-value self)) always (when (test-trn self i) (chain-match self i))))
		   ((< (length trn) (cover-value self))
		    (when (chain-match self trn) t))
		   (t
		    (when (gethash trn (trns self)) t))))
      (:trns? (if (> (length trn) (cover-value self))
		  (loop for i in (loop-wind trn (cover-value self)) always (when (test-trn self i) (chain-match self i)))
		  (when (chain-match self trn) t)))
      (otherwise nil))))
    
(defmethod search-space-in ((self mlt) (trn-sp list))
  (let* ((htal (ht (arcs self) :k))
	 (trn-sp-node (trn-match trn-sp htal))	 
	 (ll (loop for i in trn-sp-node collect (length i)))
	 (sp (loop for i in ll collect (ar-ser i)))
	 (tr (reccomb (car sp) (cdr sp)))) 
    (mapcar #'(lambda (x) (lst>trn x)) 
	    (remove nil (loop for i in tr collect 
			     (let ((trnt (loop for j in i for p from 0 
					    collect (nth j (nth p trn-sp-node)))))
			       (when (tournoi-p (lst>trn trnt) self :arcs) trnt)))))))

;------------------------------------------------------------------
;                                                    LOCATE-TOURNOI

(defgeneric locate-tournoi (self tournoi &key remanence test)
  (:documentation "<tournoi> is a ordered list of microcolonnes or a microcolonne as integer from MLT named <self>.
The key :remanence takes account of the cover-value.
The key :test manages the weights as a mean value by default."))
(defgeneric chain-match (self chain))
(defgeneric get-weight (self chain-list &key remanence test))

(defmethod chain-match ((self mlt) (chain list))
  (loop for i in (ht (trns self) :k) when (search chain i :test #'(lambda (a b) (or (eq a '?) (= a b)))) collect i))

(defun mean (xlst &optional wlst)
  (if xlst 
      (if wlst
	  (float (/ (apply #'+ (mapcar #'* xlst wlst)) (apply #'+ wlst)))
	  (float (/ (apply #'+ xlst) (length xlst))))
      0.0))

(defmethod get-weight ((self mlt) (chain-list list) &key (remanence t) (test #'mean)) ;; chain-list = result of locate-tournoi
  (if remanence
      (loop for i in chain-list collect (gethash i (trns self)))
      (loop for i in chain-list collect (funcall test (loop for i in (get-arc-from-tournoi i) collect (if (gethash i (arcs self)) (gethash i (arcs self)) 0))))))

(defun complist (a nl &optional (add 1))
  (cond ((and (numberp a) (integerp nl)) (complist (list a) nl add))
	((and (numberp a) (listp nl)) (complist (list a) (length nl) add))
	((and (listp a) (listp nl)) (complist a (length nl) add))
	((and (listp a) (integerp nl))
	 (if (<= nl (length a))
	     (subseq a 0 nl)
	     (let ((l (reverse a)))
	       (loop until (= nl (length l))
		  do
		    (push add l))
	       (reverse l))))
	(t nil)))

(defun normalize-sum (lst) (let ((sum (reduce #'+ lst))) (loop for i in lst collect (/ i sum))))

(defun loop-wind (seq wind)
  (remove nil (maplist #'(lambda (x) (if (> wind (length x)) nil (subseq x 0 wind))) seq)))

(defun merge-lw (trns-list)
  (when (loop for i from 1 to (1- (length trns-list)) always (equalp (cdr (nth (1- i) trns-list)) (butlast (nth i trns-list))))
    (append (butlast (car trns-list)) (loop for i in trns-list append (last i)))))

(defmethod locate-tournoi ((self mlt) (tournoi list) &key (remanence t) (test #'mean))
  ;; any potential tournoi not recognized as such will be interpreted as NIL including tournoi compouded of only wild cards as (? ? ...).
  (when (and (> (length tournoi) 2) (test-trn self tournoi))
    (let ((res (if remanence
		   (if (> (length tournoi) (cover-value self))
		       (let ((al (loop for i in (loop-wind tournoi (cover-value self)) collect (when (test-trn self i) (chain-match self i)))))
			 (if (member nil al)
			     nil
			     (let ((tmp (loop for i in (car al) append (remove nil (loop for j in (reccomb (list (list i)) (cdr al)) collect (merge-lw j))))))
			       (loop for r in (mapcar #'list (mapcar #'(lambda (x) (reduce #'+ (get-weight self (loop-wind x (cover-value self)) :remanence remanence :test test))) tmp) tmp) collect r))))
		       (let* ((tmp (chain-match self tournoi))
			      (lt (loop for r in (mapcar #'list (get-weight self tmp :remanence remanence :test test) tmp) collect r))
			      (htmp (make-hash-table :test #'equalp)))
			 (loop for i in lt do
			      (let* ((ind (search tournoi (cadr i) :test #'(lambda (a b) (or (eq a '?) (= a b)))))
				     (subs (subseq (cadr i) ind (+ ind (length tournoi)))))
				(setf (gethash subs htmp)
				      (if (gethash subs htmp)
					  (+ (gethash subs htmp) (car i))
					  (car i)))))
			 (loop for he in (ht htmp :al) collect (list (cdr he) (car he)))))
		   (let ((tmp (search-space-in self tournoi)))
		     (loop for r in (mapcar #'list (get-weight self tmp :remanence remanence :test test) tmp) collect r)))))
      (ordinate (mapcar #'list (mapcar #'float (normalize-sum (mapcar #'car res))) (mapcar #'cadr res)) #'> :key #'car))))

;------------------------------------------------------------------
;                                                          LEARNING    

(defgeneric add-edge (self a b)
  (:documentation "add edges in hash-table of self: 
as arcs forming the clique when self is AREA, then a = clique = LIST and b = position = INTEGER;
as arcs forming the tournoi when self is MLT, then a = tournoi = T (as integer) and b = sensorial-rate = NUMBER."))

(defgeneric set-all-zeros (self &key mode))
(defmethod set-all-zeros ((self mlt) &key mode)
  (setf (input self) (make-list (nbre-input self) :initial-element 0))
  (case mode
    (:onset (setf (mem-cache self) (list t)))
    (:fine (update-ht (fine self) (mct self) (if (net self) (sensorial-rate (id (net self))) 1))
	   (let ((lst (cons nil (mct self))))
	     (setf (mct self) (if (<= (length lst) (cover-value self)) lst (butlast lst)))))
    (otherwise (warn "The keyword should be set to :onset or :fine"))))

(defun split-mct (mct)
  (loop for i in mct until (null i) collect i))

(defmethod add-edge ((self mlt) (node integer) (sr number))
  (when (mem-cache self)
    (push node (mem-cache self))
    (when (> (length (mem-cache self)) (cover-value self))
      (update-ht (onset self) (butlast (mem-cache self)) (if (net self) (sensorial-rate (id (net self))) 1))
      (setf (mem-cache self) nil))) 
  (let* ((lst (cons node (mct self)))
	 (trn (if (<= (length lst) (cover-value self)) lst (butlast lst)))
	 (smct (split-mct trn)))
    (when (= (length smct) (cover-value self)) (update-ht (trns self) smct sr))
    (when (> (length smct) 1) (mapcar #'(lambda (l) (update-ht (arcs self) l sr)) (get-arc-from-tournoi smct)))
    (setf (mct self) trn)))

(defmethod learn :after ((self mlt) &key seq)
  (when (and seq (fanaux-list self))
    (let ((pos (position (car (last (car (nearest self (id (neuron-gagnant self)) :n-list (fanaux-list self) :d-list nil)))) (fanaux-list self))))
      (add-edge self pos (if (net self) (sensorial-rate (id (net self))) 1)))))

;------------------------------------------------------------------
;                                            NEXT-EVENT-PROBABILITY    

(defgeneric group-list (lst seg &optional mode))
(defmethod group-list ((lt list) (seg mlt) &optional mode)
  (declare (ignore seg mode))
  (let* ((rlt (mapcar #'reverse lt))
	 (np (remove-duplicates (loop for i in rlt collect (car (last (car i))))))
	 (res (loop for i in np collect
		   (loop for j in rlt when (= i (car (last (car j)))) collect (list i (cadr j)))))
	 (ult (loop for i in res collect (list (caar i) (mean (mapcar #'cadr i))))))
    (ordinate (mapcar #'list (mapcar #'car ult) (normalize-sum (mapcar #'cadr ult))) #'> :key #'cadr)))

(defun singleton (lst)
  (when (and (listp lst) (= 1 (length lst))) t))

(defgeneric next-event-probability (head self &key result remanence compute))
(defmethod next-event-probability ((head list) (self mlt) &key (result :eval) (remanence t) (compute #'rnd-weighted))
  (let ((hist
	 (if remanence
	     (locate-tournoi self (reverse (complist (cons '? (reverse head)) (cover-value self) '?)) :remanence t)
	     (locate-tournoi self (if (singleton head) (complist (cons '? head) 3 '?) (reverse (cons '? (reverse head)))) :remanence nil))))
    (when hist
      (case result
	(:prob (loop for i in (group-list hist self) collect (list (* 1.0 (cadr i)) (car i))))
	(:verbose (loop for i in (group-list hist self) do
		       (format t "~@<~S => ~3I~_~,6f %~:>~%" (car i) (* 100.0 (cadr i)))))
	(:eval (funcall compute (group-list hist self)))))))

(defmethod next-event-probability ((head integer) (self mlt) &key (result :eval) (remanence t) (compute #'rnd-weighted))
  (next-event-probability (list '? head) self :remanence remanence :result result :compute compute))

(defmethod next-event-probability ((head null) (self mlt) &key (result :eval) (remanence t) (compute #'rnd-weighted))
  (declare (ignore head))
  (let ((hist (all-tournoi self :order (cover-value self) :remanence remanence)))
    (when hist
      (case result
	(:prob (group-list hist self))
	(:verbose (loop for i in (group-list hist self) do
		       (format t "~@<~S => ~3I~_~,6f %~:>~%" (cadr i) (* 100.0 (car i)))))
	(:eval (funcall compute (group-list hist self)))))))
  
;------------------------------------------------------------------
;                                                UPDATE-COVER-VALUE    
        
(defgeneric add-prev-prob (self trn val compute))
(defmethod add-prev-prob ((self mlt) (trn list) (val integer) (compute function))
  (let ((res trn) (n (- val (cover-value self))))
    (dotimes (i n) (setf res (cons (funcall compute (group-list (mapcar #'(lambda (x) (list (car x) (reverse (cadr x)))) (locate-tournoi self (cons '? res))) self)) res))) res))

(defgeneric update-cover-value (self val &key ht compute))
(defmethod update-cover-value ((self mlt) (val integer) &key (ht (make-hash-table :test #'equalp)) (compute #'rnd-weighted))
  (cond ((or (null (cover-value self)) (= (cover-value self) val))
	 (setf (cover-value self) val))
	((> (cover-value self) val)
	 (loop for key being the hash-keys of (trns self)
	    using (hash-value value)
	    do
	      (setf (gethash (nthcdr (- (cover-value self) val) key) ht) value))
	 (setf
	  (mct self) (nthcdr (- (length (mct self)) val) (mct self))
	  (cover-value self) val
	  (trns self) ht))	  
	(t
	 (loop for key being the hash-keys of (trns self)
	    using (hash-value value)
	    do
	      (setf (gethash (add-prev-prob self key val compute) ht) value))
	 (setf
	  (mct self) (add-prev-prob self (mct self) val compute)
	  (cover-value self) val
	  (trns self) ht)))
  (values))

;------------------------------------------------------------------
