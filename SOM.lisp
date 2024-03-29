;; NEUROMUSE3
;; from neuromuse1 <https://github.com/FredVoisin/neuromuse>
;; writed by Frederic Voisin <http://www.fredvoisin.com>
;; freely adapted by Yann Ics <https://yannics.github.io>
;; LISP code to simulate artificial neural networks
;------------------------------------------------------------------

(in-package :N3)

;; make-new-symbol is used to name or rename a symbol
(defun make-new-symbol (name &optional content)
  (let ((sym (intern (string (if (boundp (read-from-string (string name)))
                               (gensym (format nil "~S-" name))
                               name)))))
    (setf (symbol-value sym) content)
    sym))

(defgeneric id (self)
  (:method (self) self)
  (:method ((self symbol)) (when (boundp self) (symbol-value self))))

;------------------------------------------------------------------
;                                                            NEURON             

(defclass neuron ()
  ((name
    :initform nil :initarg :name :accessor name)
   (net
    :initform nil :initarg :net :accessor net) ;; (RNA name which the neuron belongs)
   (ind
    :initform nil :initarg :ind :accessor ind) ;; index position in the RNA neurons-list
   (xpos
    :initform nil :initarg :xpos :accessor xpos)
   (temperature
    :initform 0.01 :initarg :temperature :accessor temperature :type number) ;; noise rate
   (synapses-list
    :initform nil :initarg :synapses-list :accessor synapses-list)
   (erreur
    :initform nil :initarg :erreur :accessor erreur) ;; current error (after activation)
   (output
    :initform '() :initarg :output :accessor output :type list)
   )) 
   
;; associate symbol at the neuron name
(defmethod initialize-instance :after ((self neuron) &key name)
  (let ((n (if name 
               (make-new-symbol name)
	       (make-new-symbol 'neuron))))
    (setf (name self) n
          (symbol-value n) self)
    n))

;; redefines lisp representation (-> print-name)
(defmethod print-object ((self neuron) stream)
  (format stream "~S" (name self) )
  (values))

(defgeneric neuron-p (self)
  (:method ((self neuron)) t)
  (:method ((self t)) nil))

(defmethod id ((self neuron)) self)

;------------------------------------------------------------------
;                                   RNA (artificial neural network)      

(defclass rna ()
  ((name
    :initform nil :initarg :name :accessor name)
   (nbre-neurons
    :initform 0 :initarg :nbre-neurons  :accessor nbre-neurons)
   (neurons-list
    :initform '() :initarg :neurons-list :accessor neurons-list)
   (nbre-input
    :initform 0 :initarg :nbre-input  :accessor nbre-input)
   (input
    :initform nil :initarg :input :accessor input)
   (radius
    :initform 0.1 :initarg :radius :accessor radius :type number) 
   (learning-rate
    :initform 0.01 :initarg :learning-rate :accessor learning-rate :type number)
   (date-report
    :initform (make-hash-table) :initarg :date-report :accessor date-report)
   (epoch
    :initform 0 :initarg :epoch :accessor epoch)
   (udp-list
    :initform '() :initarg :udp-list :accessor udp-list)
   (daemons
    :initform '() :initarg :daemons :reader daemons :accessor daemons)
   (superdaemon
   :initform '() :initarg :superdaemon :accessor superdaemon)
   ))

;; associate symbol at the rna name
(defmethod initialize-instance :after ((self rna) &key name)
  (let ((rna-name (if name 
		 (make-new-symbol name)
		 (make-new-symbol 'rna))))
    (setf (name self) rna-name
          (symbol-value rna-name) self)
    rna-name))

;; redefines lisp representation (-> print-name)
(defmethod print-object ((self rna) stream)
  (format stream "~S" (name self))
  (values))

;------------------------------------------------------------------
;                                         SOM (self organizing map)               

;; define class som
(defclass som (rna)
  ((neuron-gagnant
    :initform nil :initarg :neuron-gagnant :accessor neuron-gagnant)
   (distance-in
    :initform (lambda* (a b) (euclidean a b :position t)) :initarg :distance-in :accessor distance-in :type function) 
   (distance-out
    :initform #'euclidean :initarg :distance-out :accessor distance-out :type function)
   (voisinage
    :initform #'gauss :initarg :voisinage :accessor voisinage :type function)
   (carte
    :initform #'rnd-map :initarg :carte :accessor carte :type function)
   (field
    :initform nil :initarg :field :accessor field)
   (topology
    :initform 2 :initarg :topology :accessor topology :type integer)
   (ghost
    :initform nil :initarg :ghost :accessor ghost)
   ))

;; redefines lisp representation (-> print-name)
(defmethod print-object ((self som) stream)
  (format stream "~S" (name self))
  (values))

(defgeneric som-p (self)
  (:method ((self som)) t)
  (:method ((self t)) nil))

(defmethod id ((self som)) self)

(defgeneric is-winner-ghost (self)
  (:method ((self som)) (null (ind (id (neuron-gagnant self))))))

;------------------------------------------------------------------
;                                                    INITIALISATION       

(defgeneric init-som (self nbre-input nbre-neurons-rna &key carte topology field)
  (:documentation "Initialisation of self organizing map"))

(defvar SOM "Self Organizing Map")
(defun carlast (lst) (car (last lst)))

(defmethod init-som ((self som) (nbre-input integer) (nbre-neurons integer) &key carte topology field)
  (when carte (setf (carte self) carte))
  (when topology (setf (topology self) topology))
  (when field (setf (field self) field))
  (let* ((q-lst (funcall (carte self) self nbre-neurons))
         (nn (length q-lst)))
    (dotimes (e nn (setf (neurons-list self) (nreverse (neurons-list self))))
      (push (make-instance 'neuron
			   :name (read-from-string (string (gensym (format nil "NEURON-"))))
			   :ind e
			   :xpos (nth e q-lst)
			   :net (name self))
	    (neurons-list self)))
    (setf (nbre-neurons self) nn
	  (nbre-input self) nbre-input
	  (gethash (get-universal-time) (date-report self)) (format nil "(init-som #<RNA ~a> ~a ~a :carte #<FUNCTION ~a> :topology ~a :field ~a)" self nbre-input nbre-neurons (let ((mvl (multiple-value-list (function-lambda-expression (carte self))))) (cond ((listp (carlast mvl)) (format nil "~S" (if (ml? (carte self)) (ml! (carte self)) (carte self)))) (t (format nil "~S" (carlast mvl))))) (topology self) (field self))
	  (ghost self) (make-instance 'neuron :name 'ghost :net (name self))))
  (values self))

(defmethod init-som :after ((self som) (nbre-input integer) (nbre-neurons integer) &key carte topology field)
  (declare (ignore carte topology field))
  (dolist (e (neurons-list self))
    (setf (output e) (loop repeat nbre-input collect (random 1.0))))
  (values self))

;------------------------------------------------------------------
;                                                        ACTIVATION     

(defgeneric activation (self &key seq))
(defgeneric winner (self))

(defmethod activation ((self neuron) &key seq)
  (declare (ignore seq))
  (let ((net (id (net self))))
    (setf (erreur self) (funcall (distance-out net) net self))
    (values)))

(defmethod activation ((self som) &key seq)
  (declare (ignore seq))
  (dolist (n (neurons-list self))
    (activation n))
  (not (zerop (reduce #'+ (input self)))))

(defmethod winner ((self som))
  (if (activation self)
      (loop for j in (neurons-list self) 
	 when (= (erreur j) (loop for i in (neurons-list self) minimize (erreur i)))
	 return j)
      (ghost self)))

;------------------------------------------------------------------
;                                                          LEARNING    
                  
(defgeneric learn (self &key seq))

(defmethod learn ((self som) &key seq)
  (setf (neuron-gagnant self)
	(winner self))
  ;; neighbourhood correction
  (unless (or seq (is-winner-ghost self))
    (loop for n in (neurons-list self) do   
	 (let ((dist (funcall (distance-in self) (id (neuron-gagnant self)) n))
	       (sl))
	   (when (<= dist (radius self))
	     (let ((correction (funcall (voisinage self) dist (radius self) (learning-rate self))))
	       (dotimes (i (nbre-input self) (setf (output n) (nreverse sl)))
		 (push (+ (nth i (output n)) 
			  (* correction
			     (- (nth i (input self))
				(nth i (output n))))) sl)))))) 
    (setf (epoch self) (1+ (epoch self))))
  (values))

;------------------------------------------------------------------
