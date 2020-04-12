;; NEUROMUSE3
;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                              AREA   

(defclass area () 
  ((name
    :initform nil :initarg :name :accessor name)
   (soms-list
    :initform '() :initarg :soms-list :accessor soms-list :type list)
   (fanaux-length
    :initform '() :initarg :fanaux-length  :accessor fanaux-length :type list)
   (current-clique
    :initform nil :initarg :current-clique :accessor current-clique)
   ;(valence
   ; :initform nil :initarg :valence :accessor valence)
   (sensorial-rate
    :initform 1 :initarg :sensorial-rate :accessor sensorial-rate :type number)
   (arcs
    :initform (make-hash-table :test #'equalp) :initarg :arcs :accessor arcs)
   (date-report
    :initform (make-hash-table) :initarg :date-report :accessor date-report)
   (udp-list
    :initform '() :initarg :udp-list :accessor udp-list :type list)
   ))

;; associate symbol at the area name
(defmethod initialize-instance :after ((self area) &key name)
  (let ((ar (if name 
               (make-new-symbol name)
	       (make-new-symbol 'area))))
    (setf (name self) ar
          (symbol-value ar) self)
    ar))

;; redefines lisp representation (-> print-name)
(defmethod print-object ((self area) stream)
  (format stream "~S" (name self) )
  (values))

(defgeneric area-p (self))

(defmethod area-p ((self area)) t)
(defmethod area-p ((self t)) nil)

(defmethod id ((self area)) self)

;; to update-fanaux
(defmethod update-indice ((self area) (edge-list list) (new-fanaux-list list) (surjection list) &optional som-position)
  (when som-position
    (labels ((update-i (indice-fanal surjection)
	       (position (nth (random (length (nth indice-fanal surjection))) (nth indice-fanal surjection)) (loop for f in new-fanaux-list collect (id f)) :test #'equalp)))
      (loop for c in edge-list collect (if (= (cadr c) som-position) (list (update-i (car c) surjection) (cadr c)) c)))))

;------------------------------------------------------------------
;                                             INITIALISATION (AREA)  

(defgeneric init-area (self soms-list)
  (:documentation "Initialisation of an area for self organizing maps"))

(defun mk-mlt-symbol-lst (lst &optional r)
  (loop for i in lst do (push (id i) r)) (reverse r))

(defmethod init-area ((self area) (soms-list list))
  (when (loop for s in soms-list always (mlt-p (id s)))
    (setf (soms-list self) soms-list
	  (gethash (get-universal-time) (date-report self)) (format nil "(init-area #<AREA ~a> #<SL (~{~a~^ ~})>)" self soms-list)
	  (fanaux-length self) (loop for i in soms-list collect (length (fanaux-list (id i)))))
    ;; for now the area name is display in each mlt net slot as self.
    ;; This instance will not work if one mlt needs to relate to another area.  
    ;; This will be part of further development ...  
    (loop for i in soms-list 
       do (setf (net (id i)) self))
    self))

(defvar *available-area* '())

(defun soms-list-p (lst) 
  (ignore-errors (loop for i in lst always (mlt-p (id i)))))

(defun create-area (name soms-list)
  (if (soms-list-p soms-list)
      (progn
	(push (init-area (make-instance 'area :name name) soms-list) *available-area*)
	(eval (list 'defvar (read-from-string (format nil "~S" (car *available-area*))) (car *available-area*))))
      (warn "To create AREA, every item of the soms-list has to be a MLT. Please check *available-som*.")))

;------------------------------------------------------------------
;                                                 ACTIVATION (AREA)   

(defmethod activation ((self area) &key seq)
  (unless (loop for i in (soms-list self) always (and (not (null (fanaux-list (id i)))) (zerop (apply #'+ (input (id i))))))
    (loop for i in (soms-list self) do (learn (id i) :seq seq))
    (setf (current-clique self)
	  (loop for s in (soms-list self) collect
	       (car (mct (id s)))))))

;------------------------------------------------------------------
;                                                   LEARNING (AREA)  

(defmethod set-all-zeros ((self area) &key mode)
  (loop for i in (soms-list self) do (set-all-zeros (id i) :mode mode)))

;;------------------------------
(defgeneric read-data (self data &key scale)
  (:method ((self mlt) (data list) &key scale) (when (loop for i in data always (= (nbre-input self) (length i))) (if scale (scaling data :mlt self) data)))
  (:method ((self mlt) (file string) &key scale) (read-data self (read-file file) :scale scale))
  (:method ((self mlt) (file pathname) &key scale) (read-data self (read-file (namestring file)) :scale scale))
  (:method ((self mlt) (data null) &key scale) (declare (ignore self data scale)) nil)
  (:method ((self mlt) (data t) &key scale) (declare (ignore self data scale)) nil)
  (:method ((self area) (data list) &key scale)
    (let ((seq (remove nil (loop for s in (soms-list self) for f in data collect (read-data (id s) f :scale scale)))))
      (when (and (= (length (soms-list self)) (length seq)) (loop for i in (cdr seq) always (= (length (car seq)) (length i)))) seq))))
;;------------------------------

(defmethod add-edge ((self area) (clique list) (pos integer))
  (let* ((l (loop for i in clique for j from 0 when (integerp i) collect (list i j)))
	 (cliq (remove (nth pos l) (copy-list l) :test #'equalp))	 
	 (node (nth pos l)))
    (loop for i in cliq 
	 do
	 (update-ht (arcs self) (list node i) (sensorial-rate self)))))

(defmethod learn ((self area) &key seq)
  (if seq
      (let ((data (when seq (read-data self seq :scale t))))
	(if data
	    (progn
	      (set-all-zeros self :mode :onset)
	      (loop
		 for i from 0 to (1- (length (car data))) do
		   (loop
		      for s in (soms-list self)
		      for d in data do
			(setf (input (id s)) (nth i d))) (learn self))
	      (set-all-zeros self :mode :fine))
	    (warn "The argument of the key :seq is not a valid list.")))
      (when (activation self :seq t)
	(dotimes (n (length (soms-list self))) (add-edge self (current-clique self) n)))))
    
;------------------------------------------------------------------
;                                                     LOCATE-CLIQUE

(defgeneric locate-clique (self nodes &key test)
  (:documentation "<nodes> is a list of node(s) supposed to form a clique from AREA named <self>."))

(defun all-pairs (lst &key with-index) 
  (if (> (length lst) 1)
      (let* ((l (if with-index (loop for a in lst for pos from 0 collect (list a pos)) lst)))
	(flat-once (loop for i from 0 to (- (length lst) 2)
		      collect (mapcar #'(lambda (x) (list (car (nthcdr i l)) x)) (cdr (nthcdr i l))))))
      lst))

(defun clique-p (clique area)
  "clique is a list of indexes of each fanal according the soms-list of area.
In others word, clique = (index_fanal_SOM1 index_fanal_SOM2 ...)."
  (loop for i in (all-pairs clique :with-index t) always (gethash i (arcs (id area)))))

(defun sort-clique (lst)
  (ordinate lst #'< :key #'(lambda (x) (if (integerp (cadr x)) (cadr x) (cadar x)))))

(defun group-edges (lst &optional r1 r2)
  (let ((sl (ordinate lst #'> :key #'cadr))) 
    (loop for i in sl do
	 (if (or (null r1) (eq (cadr i) (cadar r1))) (push i r1)
	     (progn (push r1 r2) (setf r1 '()) (push i r1))))
    (push r1 r2)
    r2))

(defun count-item-in-list (lst &optional r)
  (dolist (e (remove-duplicates lst :test #'equalp) r)
    (push (list (count e lst :test #'equalp) e) r)))

(defun ordered-combinatorial-distribution (lst)
  (if (> (length lst) 1)
      (let ((l (loop for i in lst collect (if (listp i) i (list i)))))
	(reccomb (car l) (cdr l))) lst))

(defmethod get-weight ((self area) (chain-list list) &key remanence (test #'mean))  ;; chain-list = (ordered-combinatorial-distribution (result-of-locate-clique))
  (declare (ignore remanence))
  (let ((tmp (loop for i in chain-list collect (loop for j in (all-pairs i :with-index t) collect (if (gethash j (arcs self)) (gethash j (arcs self)) 0))))) 
    (loop for c in tmp collect (unless (member 0 c) (funcall test c)))))

;; some possible test
(defun sum (lst) (reduce #'+ lst))
;; see also mini or maxi

(defmethod group-list ((lt list) (seg area) &optional mode)
  (declare (ignore seg mode))
  (let ((ll (remove-duplicates lt :test #'equalp)))
    (ordinate (mapcar #'list (mapcar #'cadr ll) (normalize-sum (mapcar #'car ll))) #'> :key #'cadr)))

(defgeneric test-clique (self clique &key as-nodes))
(defmethod test-clique ((self area) (clique list) &key as-nodes)
  (if as-nodes
      (and clique (loop for i in clique always (= 2 (length i)))
	   (let ((cli (make-list (length (soms-list self)) :initial-element '?)))
	     (loop for i in clique	 
		do (setf cli (substitute-nth (car i) (cadr i) cli)))
	     (test-clique self cli)))
      (and (= (length (soms-list self)) (length clique)) (loop for c in clique for i from 0 always (or (eq '? c) (and (integerp c) (>= c 0) (< c (nth i (fanaux-length self)))))) (not (loop for i in clique always (eq '? i))))))

(defmethod locate-clique ((self area) (nodes list) &key (test #'mean))
  ;; any malformed nodes list will be interpreted as NIL and will return all possible cliques of self.
  (let* ((el (if (listp (car nodes)) (when (test-clique self nodes :as-nodes t) nodes) (when (test-clique self nodes) (loop for i in nodes for s from 0 when (integerp i) collect (list i s)))))
	 (nht (arcs self))
	 (ed (if (null el)
		 (ht nht :k)
		 (loop for i in el collect (loop for key being the hash-keys of nht when (equalp i (car key)) collect key))))
	 (cil (count-item-in-list (flat-once (flat-once (if (null el) (list ed) ed)))))
	 (ped (loop for i in cil when (and (not (member (cadr i) el :test #'equalp)) (>= (car i) (length el))) collect (cadr i)))
	 (eds (loop for i in ped when (= 1 (count (cadr i) ped :key #'cadr)) collect i))
	 (edp (loop for i in ped unless (= 1 (count (cadr i) ped :key #'cadr)) collect i))
	 (r (if edp
		(sort-clique (append el eds (group-edges edp)))
		(sort-clique (append el eds))))
	 (res (ordered-combinatorial-distribution (loop for i in r collect (if (listp (car i)) (mapcar #'car i) (car i)))))) 
    (unless (integerp (car res)) (mapcar #'reverse (ordinate (group-list (loop for r in (mapcar #'list (get-weight self res :test test) res) unless (null (car r)) collect r) self) #'> :key #'cadr)))))

;------------------------------------------------------------------
;                                            NEXT-EVENT-PROBABILITY

(defun dispatch-combination (seqs &optional seq count res)
  (if (and seq count (= count (1- (length seqs))))
      res
      (if res
	  (dispatch-combination seqs (nth (+ 1 count) seqs) (+ 1 count) (reccomb res (list (nth (+ 1 count) seqs))))
	  (dispatch-combination seqs (nth 0 seqs) 1 (reccomb (car seqs) (list (nth 1 seqs)))))))

(defun trns-prob (clique al)
  (reduce #'* (mapcar #'cadr (loop for i in clique for j in al when j collect (assoc i (mapcar #'reverse j))))))

(defun mat-trans (lst)
  (apply #'mapcar #'list lst))

(defmethod next-event-probability ((head list) (self area) &key (result :compute) remanence (compute #'rnd-weighted))
  (let ((al (loop for i in (mat-trans head) for net in (soms-list self) collect (next-event-probability i (id net) :remanence remanence :result :list))) 
	r) 
    (loop for c in (dispatch-combination (mapcar #'list! (loop for l in al collect (if (null l) '? (mapcar #'cadr l))))) when (test-clique self c) do (setf r (append (mapcar #'cadr (locate-clique self c)) r)))
    (let* ((tmp (group-list (mat-trans (list (get-weight self r) r)) self))
	   (rwi (mapcar #'cons (loop for i in tmp collect (trns-prob (car i) al)) (mapcar #'reverse tmp)))
	   (orwi (ordinate rwi #'> :key #'car))) 
      (case result
	(:list (if remanence orwi rwi))
	(:verbose (loop for i in (if remanence orwi rwi) do
		       (format t "~@<~S => ~3I~_R:~,6f % - ~,6f %~:>~%" (caddr i) (* 1.0 (car i)) (* 1.0 (cadr i)))))
	(:compute (let* ((res (funcall compute (if remanence (mapcar #'reverse (mapcar #'list (mapcar #'car orwi) (mapcar #'caddr orwi))) tmp)))
			 (vals (assoc res (mapcar #'reverse rwi) :test #'equalp)))
		    (when res (values
			       res
			       (caddr vals)
			       (cadr vals)))))))))

(defmethod next-event-probability ((head null) (self area) &key result remanence compute)
  (declare (ignore head self result remanence compute)))

;------------------------------------------------------------------
