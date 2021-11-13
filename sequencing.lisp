;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                        SEQUENCING

(defclass sequencing () 
  ((name
    :initform nil :initarg :name :accessor name)
   (dub
    :initform nil :initarg :dub :accessor dub)
   (description
    :initform "" :initarg :description :accessor description :type string)
   (net
    :initform nil :initarg :net :accessor net)
   (buffer-in
    :initform nil :initarg :buffer-in :accessor buffer-in :type list)
   (buffer-out
    :initform nil :initarg :buffer-out :accessor buffer-out :type list)
   (dyn-buffer
    :initform nil :initarg :dyn-buffer :accessor dyn-buffer)
   (buffer-size
    :initform 30 :initarg :buffer-size :accessor buffer-size :type integer)
   (pulse
    :initform 1 :initarg :pulse :accessor pulse :type number)
   (sync
    :initform nil :initarg :sync :accessor sync)
   (outset
    :initform t :initarg :outset :accessor outset)
   (pattern
    :initform nil :initarg :pattern :accessor pattern)
   (rule
    :initform nil :initarg :rule :accessor rule)
   (tag
    :initform "N3" :initarg :tag :accessor tag)
   (meter
    :initform 4 :initarg :meter :accessor meter :type number)
   (remanence
    :initform nil :initarg :remanence :accessor remanence)
   (odds
    :initform #'rnd-weighted :initarg :odds :accessor odds :type function)
   (routine
    :initform nil :initarg :routine :accessor routine)
   (offset
    :initform 0 :initarg :offset :accessor offset)
   (counter
    :initform 0 :initarg :counter :accessor counter)
   (udp-list
    :initform nil :initarg :udp-list :accessor udp-list)
   (mem-cache
    :initform nil :initarg :mem-cache :accessor mem-cache)
   ))

(defmethod initialize-instance :after ((self sequencing) &key name)
  (let ((ar (if name 
               (make-new-symbol name)
	       (make-new-symbol 'sequencing))))
    (setf (name self) ar
          (symbol-value ar) self)
    ar))

;; redefines lisp representation (-> print-name)
(defmethod print-object ((self sequencing) stream)
  (format stream "~S" (name self) )
  (values))

(defgeneric sequencing-p (self)
  (:method ((self sequencing)) t)
  (:method ((self t)) nil))

(defmethod id ((self sequencing)) self)

;------------------------------------------------------------------
;                                       INITIALISATION (SEQUENCING)  

(defvar *all-sequencing* '())

(defun init-sequencing (&key dub description net buffer-size pulse sync pattern rule tag meter remanence odds routine osc)
  (push (make-instance 'sequencing :name 'sequencing) *all-sequencing*)
  (let ((seq (car *all-sequencing*)))
    (setf (dub seq) (if dub dub (name seq)))
    (when description (setf (description seq) description))
    (when net (setf (net seq) net))
    (when buffer-size (setf (buffer-size seq) buffer-size))
    (when pulse (setf (pulse seq) pulse))
    (when sync (setf (sync seq) sync))
    (when pattern (setf (pattern seq) pattern))
    (when rule (setf (rule seq) rule))
    (when tag (setf (tag seq) tag))
    (when meter (setf (meter seq) meter))
    (when remanence (setf (remanence seq) remanence))
    (when odds (setf (odds seq) odds))
    (when routine (setf (routine seq) routine))
    (when osc (setf (udp-list seq) osc)) 
    (eval (list 'defparameter (read-from-string (format nil "~S" (dub seq))) seq))))

(defun create-sequencing (&key dub net description pattern pulse rule odds meter buffer-size remanence osc tag)
  (init-sequencing :dub dub :net net :description description :pulse pulse :pattern pattern :rule rule :odds odds :meter meter :remanence remanence :buffer-size buffer-size :osc osc :tag tag))

(defmethod copy-sequencing ((self sequencing) &key dub description net buffer-size pulse sync pattern rule tag meter remanence odds osc)
  (init-sequencing
   :dub dub
   :description (if description description (format nil "~A dub:~A copy" (name self) (dub self)))
   :net (if net net (net self))
   :buffer-size (if buffer-size buffer-size (buffer-size self))
   :pulse (if pulse pulse (pulse self))
   :sync (if sync sync (sync self))
   :pattern (if pattern pattern (pattern self))
   :rule (if rule rule (rule self))
   :tag (if tag tag (tag self))
   :meter (if meter meter (meter self))
   :remanence (if remanence remanence (remanence self))
   :odds (if odds odds (odds self))
   :osc (if osc osc (udp-list self))
   :routine (routine self)))

;------------------------------------------------------------------
;                                                            THREAD   

;; ... from bordeaux-threads package ... ;;
(defun %threadp (object)                 ;;
  #+sbcl(typep object 'sb-thread:thread) ;;
  #+openmcl(typep object 'ccl:process))  ;;
                                         ;;
(defun %all-threads ()                   ;;
  #+sbcl(sb-thread:list-all-threads)     ;;
  #+openmcl(ccl:all-processes))          ;;
;; ..................................... ;;

(defun %preset-thread (function name &optional postpend)
  #+sbcl(sb-thread:make-thread function :name (format nil "~S-~S" name (if (symbolp postpend) postpend 'thread)))
  #+openmcl(ccl:process-preset (ccl:make-process (format nil "~S-~S" name (if (symbolp postpend) postpend 'thread))) function name))

(defun %enable-thread (thread)
  #+sbcl(declare (ignore thread))
  #+openmcl(ccl:process-enable thread))

#+sbcl(defparameter %current-thread sb-thread:*current-thread*)
#+openmcl(defparameter %current-thread ccl:*current-process*)

(defun %initform-thread (thread)
  #+sbcl(declare (ignore thread)) 
  #+openmcl(car (ccl::process-initial-form thread)))

(defun %pause-thread (thread)
  #+sbcl(declare (ignore thread))
  #+openmcl(ccl:process-suspend thread))

(defun %resume-thread (thread)
  #+sbcl(declare (ignore thread))
  #+openmcl(ccl:process-resume thread))

(defun %kill-thread (thread)
  #+sbcl(sb-thread:terminate-thread thread)
  #+openmcl(ccl:process-kill thread))

;------------------------------------------------------------------
;                                                             CLOCK   

(defvar +beat+ 1)
(defvar +clock+ 0)
#+sbcl(defvar ++clock++ (sb-thread:make-thread #'(lambda () (loop do (incf +clock+) (sleep *latency*))) :name "++clock++"))
#+openmcl(defvar ++clock++ (ccl:process-run-function "++clock++" #'(lambda () (loop do (incf +clock+) (sleep *latency*)))))

;------------------------------------------------------------------
;                                                               SET   

(defgeneric clique>xpos (self clique)
  (:method ((self mlt) (clique list)) (clique>xpos (id (net self)) clique))
  (:method ((self area) (clique list))
    (when (test-clique self clique)
      (loop for i in clique for n in (soms-list self) collect (xpos (id (nth i (fanaux-list (id n)))))))))

(defun dispatch-udp-list (udp-list &key verbose)
  (if verbose (format t "~{IP:~{~A~^:~}~&~}~&" (dispatch-udp-list udp-list))
      (progn (when (integerp (car (list! udp-list))) (setf udp-list (cons "127.0.0.1" (list! udp-list))))
	     (if udp-list
		 (let ((ind (loop for i in udp-list for pos from 0 when (stringp i) collect pos)))
		   (flat-once (mapcar #'(lambda (x) (reccomb (list (car x)) (list (cdr x))))
				      (loop for x in ind for next from 1
					 collect (subseq udp-list x (nth next ind))))))
		 (list (list "127.0.0.1" 7771))))))

(defgeneric bo (self &optional ind)
  (:method ((self sequencing) &optional ind)
    (cond ((integerp ind) (nth (1- ind) (car (last (buffer-out self)))))
	  ((and (eq ind :xpos) (or (area-p (id (net self))) (area-p (id (net (id (net self)))))))
	   (flatten (clique>xpos (id (net self)) (car (last (buffer-out self))))))
	  (t (car (last (buffer-out self)))))))

(defgeneric sync>pulse (self)
  (:method ((self sequencing))
    (setf (sync self)
	  (if (integerp (sync self))
	      (cons (sync self) (/ (* (meter self) +beat+) (sync self)))
	      (let ((val (1- (loop for i from 1 when (> (* i (pulse self)) (meter self)) return i))))
		(cons val (/ (* (meter self) +beat+) val)))))))

(defgeneric sync! (self)
  (:method ((self sequencing))
    (cond  ((and (outset self) (zerop (mod (/ +clock+ (* (pulse self) (/ +beat+ *latency*))) (meter self)))) (setf (outset self) nil) (incf (counter self)))
	   ((and (listp (sync self)) (integerp (rationalize (mod (/ +clock+ (* (cdr (sync self)) (/ +beat+ *latency*))) (meter self))))) (when (zerop (mod (/ +clock+ (* (pulse self) (/ +beat+ *latency*))) (meter self))) (incf (counter self))) (>= +clock+ (offset self))))))

(defmacro set-routine (self &body funcs)
  "Managing buffer-out with dyn-buffer... 
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     (when (%threadp (routine ,self)) (%kill-thread (routine ,self)))
     (format t "OSC will send message to:~&")
     (dispatch-udp-list (udp-list ,self) :verbose t)
     (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag ,self)))))
     (setf (routine ,self) (lambda* (self) 
				    (loop do
					 (when (< (length (buffer-out self)) (buffer-size self))
					   ,@funcs ;; should be a thread to avoid delay osc???
					   (push (car (last (dyn-buffer self))) (buffer-out self)))
					;-------------------------------------------------------
					 (when (and (buffer-out self) (if (sync self) (sync! self) (>= +clock+ (offset self))))
					   
					   (let ((pulse
						  (cond ((null (sync self)) (* +beat+ (pulse self) (1+ (bo self 1))))
							((or (listp (sync self)) (sync>pulse self)) (* (cdr (sync self)) (1+ (bo self 1))))
							(t (error "See pulse cond in your routine.")))))

					     (when *debug* (format #.*standard-output* "~S~&" (buffer-out self)))
					     
					     (loop for ip in (dispatch-udp-list (udp-list self)) 
						do (send-udp
						    (read-from-string
						     (format nil "(\"/~S\" ~{\"~S\"~})"
							     (read-from-string (remove #\/ (string (tag self))))
							     (append (bo self) (bo self :xpos) (list pulse)))) 
						    (car ip) (cadr ip)))
					     (setf (offset self) (+ (/ pulse *latency*) +clock+)
						   (buffer-out self) (butlast (buffer-out self)))))
					;-------------------------------------------------------
					 (sleep *latency*))))))

;------------------------------------------------------------------
;                                                              PLAY   

(defgeneric kill-routine (self))
(defgeneric act-routine (self))
(defgeneric pause-routine (self))
(defgeneric resume-routine (self))

(defmethod kill-routine ((self sequencing))
  (let ((cpt (routine self)))
    (when (%threadp (routine self)) (setf cpt (%initform-thread (routine self))) (%kill-thread (routine self)))
    (setf (outset self) t
	  (routine self) cpt
	  (counter self) 0
	  (buffer-out self) nil
	  (dyn-buffer self) nil)))

(defmethod act-routine ((self sequencing))
  (kill-routine self)
  (if (functionp (routine self)) (setf (routine self) (%preset-thread (routine self) self 'routine)) (error "No assigned function! use > (set-routine &progn funcs)")) 
  (%enable-thread (routine self))
  (format t "~S is playing on:~&" (name self))
  (dispatch-udp-list (udp-list self) :verbose t)
  (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag self))))))

(defmethod pause-routine ((self sequencing))
  (when (%threadp (routine self))
    (setf (outset self) t) (%pause-thread (routine self))))

(defmethod resume-routine ((self sequencing))
  (when (%threadp (routine self))
    (%resume-thread (routine self))))

;------------------------------------------------------------------
;                                                              SAVE   

(defmethod save ((self sequencing))
  (let ((path (format nil "~A~A.seq" *N3-BACKUP-DIRECTORY* (remove #\* (string (dub self))))))
    (let ((slots-sequencing (get-slots self)))
      (with-open-file (stream path
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream "(PUSH (MAKE-INSTANCE (QUOTE N3::SEQUENCING)")
	(loop for s in slots-sequencing do
	     (let ((val (funcall s self)))
	       (cond ((%threadp val) (format stream " :~S ~S" s (ml! (%initform-thread val))))
		     ((functionp val)
		      (let ((mvl (multiple-value-list (function-lambda-expression val))))
			(cond
			  ((listp (car (last mvl))) (format stream " :~S ~S" s (if (ml? val) (ml! val) val)))				
			  (t (format stream " :~S #'~S" s (car (last mvl)))))))
		     ((or (eq s (read-from-string "NAME"))
			  (eq s (read-from-string "BUFFER-IN"))
			  (eq s (read-from-string "BUFFER-OUT"))
			  (eq s (read-from-string "DYN-BUFFER"))
			  (eq s (read-from-string "OFFSET"))
			  (eq s (read-from-string "COUNTER")))
			  (format stream ""))
		     (t (format stream " :~S (QUOTE ~S)" s val)))))
	(format stream ") N3::*ALL-SEQUENCING*)")
	(format stream " (DEFVAR ~S (CAR *ALL-SEQUENCING*))" (dub self))))
    (UIOP:run-program (format nil "sh -c '~S ~S'" *UPDATE-SAVED-NET* path))))

(defun sequencing-menu ()
  (let* ((stdout (format nil "~a" (UIOP:run-program (format nil "sh -c 'cd ~S; for f in *.seq; do echo $f; done'" *N3-BACKUP-DIRECTORY*) :output :string)))
	 (items (string-to-list stdout)))
    (when items
      (format t ";---------------")
      (dotimes (item-number (length items))
	(format t "~%~A: ~A" (+ item-number 1) (nth item-number items)))
      (format t "~%Load (Type any key to escape): ")
      (let ((val (read)))
	(when (and (integerp val) (<= val (length items)) (> val 0))
	  ;; add warning if already loaded
	  ;; + warning if net is not loaded
	  (load (concatenate 'string *N3-BACKUP-DIRECTORY* (string (car (split-symbol (nth (- val 1) items)))) ".seq"))
	  (format t "~45<~A[~A] ...~;... loaded ...~>~%" (name (car *all-sequencing*)) (dub (car *all-sequencing*))))))))

;------------------------------------------------------------------
;                                                           COMPUTE   

(defgeneric markov-chain (self net &optional buffer)
  (:method ((self sequencing) (net area) &optional (buffer (dyn-buffer self)))
    (if buffer
	(progn
	  (if (or (numberp (car (last buffer))) (loop for i in (car (last buffer)) never (eq '? i)))
	      ;; compute next clique from the previous with rule
	      (setf buffer (reverse (cons (funcall (rule self) self) (if (numberp (car (last buffer))) (reverse (butlast buffer)) (reverse buffer)))))
	      ;; if next-event-probability=nil remove oldest clique
	      (setf buffer (cdr buffer)))
	  ;; locate the last clique
	  (let ((nc (next-event-probability buffer net :result :eval :remanence (remanence self) :opt :buffer :compute (odds self))))
	    (when nc
	      (setf buffer (reverse (cons nc (reverse (butlast buffer))))))
	    (if (member '? (car (last buffer)))
		(markov-chain self net buffer)	
		(setf (dyn-buffer self) buffer))))
	(setf (dyn-buffer self) (list (next-event-probability nil net :result :eval :remanence (remanence self) :compute (odds self))))))
  (:method ((self sequencing) (net mlt) &optional (buffer (dyn-buffer self)))
    (markov-chain self (id (net net)) buffer)))

(defgeneric cycle-chain (self net &optional cycle)
  (:method ((self sequencing) (net mlt) &optional cycle)
    ;; the cycle is stored in (pattern self)
    (unless (pattern self) (setf (pattern self) cycle))
    ;; initiate (dyn-buffer self)
    (unless (dyn-buffer self) (setf (dyn-buffer self) (list (next-event-probability (list (list (car (nth (mod (counter self) (length (pattern self))) (pattern self))) '? '?)) (id (net (id (net self)))) :result :eval :opt :buffer :compute (odds self) :remanence (remanence self))))) 
    ;; evaluation is a fanal indice stored in (dyn-buffer self)
    (setf (dyn-buffer self) (reverse (cons (next-event-probability (car (nth (mod (counter self) (length (pattern self))) (pattern self))) net :remanence (remanence self) :opt (nth (mod (counter self) (length (pattern self))) (pattern self)) :compute (odds self)) (reverse (dyn-buffer self)))))))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(make-instance 'neuron :name 'neuron)
(make-instance 'neuron :name 'ghost)
(make-instance 'area :name 'area)
(make-instance 'sequencing :name 'sequencing)
(defconstant MLT (make-instance 'mlt))
(format t "; MLT default functions:~&; DISTANCE-IN: ~S~&; DISTANCE-OUT: ~S~&; VOISINAGE: ~S~&; CARTE: ~S~&" (DISTANCE-IN MLT) (DISTANCE-OUT MLT) (VOISINAGE MLT) (CARTE MLT))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
