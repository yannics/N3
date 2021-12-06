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
    :initform (make-hash-table :test #'equalp) :initarg :mem-cache :accessor mem-cache)))

(defvar +routine+ nil)

(defmethod initialize-instance :after ((self sequencing) &key name)
  (let ((ar (if name 
               (make-new-symbol name)
	       (make-new-symbol 'sequencing))))
    (setf (name self) ar
          (symbol-value ar) self)
    (setf (gethash 'routine-initform (mem-cache self)) +routine+)
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

(defun init-sequencing (&key dub description net buffer-size pulse sync pattern tag meter remanence odds osc)
  (push (make-instance 'sequencing :name 'sequencing) *all-sequencing*)
  (let ((seq (car *all-sequencing*)))
    (setf (dub seq) (if dub dub (name seq)))
    (when description (setf (description seq) description))
    (when net (setf (net seq) net))
    (when buffer-size (setf (buffer-size seq) buffer-size))
    (when pulse (setf (pulse seq) pulse))
    (when sync (setf (sync seq) sync))
    (when pattern (setf (pattern seq) pattern))
    (when tag (setf (tag seq) tag))
    (when meter (setf (meter seq) meter))
    (when remanence (setf (remanence seq) remanence))
    (when odds (setf (odds seq) odds))
    (when osc (setf (udp-list seq) osc)) 
    (eval (list 'defparameter (read-from-string (format nil "~S" (dub seq))) seq))))

(defun create-sequencing (&key dub net description pulse pattern odds meter sync remanence buffer-size osc tag)
  (init-sequencing :dub dub :net net :description description :pulse pulse :pattern pattern :odds odds :meter meter :sync sync :remanence remanence :buffer-size buffer-size :osc osc :tag tag))

;------------------------------------------------------------------
;                                                            THREAD   

;; /!\ currently works only on ccl64

#+openmcl(progn
	   (defun _threadp (object) (typep object 'ccl:process)) 
	   (defun _all-threads () (ccl:all-processes))
	   (defun _preset-thread (function name) (ccl:process-preset (ccl:make-process name) function name))
	   (defun _enable-thread (thread) (ccl:process-enable thread))
	   (defun _make-thread (function name) (_enable-thread (_preset-thread function name)))
	   (defun _thread-zombie (thread) (ccl:process-exhausted-p thread))
	   (defun _initform-thread (thread) (car (ccl::process-initial-form thread)))
	   (defun _pause-thread (thread) (ccl:process-suspend thread))
	   (defun _resume-thread (thread) (ccl:process-resume thread))
	   (defun _kill-thread (thread) (ccl:process-kill thread)))

(defgeneric check-thread (self)
  (:method ((self sequencing))
    (cond ((and (_threadp (gethash 'compute (mem-cache self))) (_thread-zombie (gethash 'compute (mem-cache self)))) (format t ";compute -> thread zombie ~S" (gethash 'compute (mem-cache self))))
	  ((and (_threadp (gethash 'compute (mem-cache self))) (not (_thread-zombie (gethash 'compute (mem-cache self))))) (format t ";compute -> thread running -- buffers ~S ~S" (if (= (buffer-size self) (length (buffer-out self))) 'filled 'filling) (gethash 'compute (mem-cache self))))
	  (t (format t ";compute -> thread NIL")))
    (cond ((and (_threadp (gethash 'routine (mem-cache self))) (_thread-zombie (gethash 'routine (mem-cache self)))) (format t "~&;routine -> thread zombie ~S" (gethash 'routine (mem-cache self))))
	  ((and (_threadp (gethash 'routine (mem-cache self))) (not (_thread-zombie (gethash 'routine (mem-cache self))))) (format t "~&;routine -> thread running ~S" (gethash 'routine (mem-cache self))))
	  (t (format t "~&;routine -> thread NIL")))))

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
    (let ((buf (if (buffer-out self) (car (last (buffer-out self))) (car (last (dyn-buffer self))))))
      (cond ((integerp ind) (nth (1- ind) buf))
	    ((and (eq ind :xpos) (or (area-p (id (net self))) (area-p (id (net (id (net self)))))))
	     (flatten (clique>xpos (id (net self)) buf)))
	    (t buf)))))

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
  "Managing buffer-out with dyn-buffer according to the rule(s) in (rule self)... 
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     (if (rule ,self)
	 (progn
	   (when (_threadp (gethash 'routine (mem-cache ,self))) (_kill-thread (gethash 'routine (mem-cache ,self))) (remhash 'routine (mem-cache ,self)))
	   (when (_threadp (gethash 'compute (mem-cache ,self))) (_kill-thread (gethash 'compute (mem-cache ,self))) (remhash 'compute (mem-cache ,self)))
	   (setf (routine ,self)
		 (lambda* (self)
		   (loop do
		     (when (< (length (buffer-out self)) (buffer-size self)) ,@funcs)
		     (sleep *latency*)))
		 (gethash 'compute (mem-cache ,self))
		 (_make-thread (routine ,self) ,self))
	   (format t "OSC will send message to:~&")
	   (dispatch-udp-list (udp-list ,self) :verbose t)
	   (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag ,self))))))
	 (warn "Set rule first..."))))

(defmacro set-rule (self &body funcs)
  "The last function have to return a partial clique or tournoi according to the involved net...
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     (setf (rule ,self) (lambda* (self) ,@funcs))))

(setf +routine+
      (lambda* (self)
	       (loop do (when (and (dyn-buffer self) (if (sync self) (sync! self) (>= +clock+ (offset self))))					   
			  (let ((pulse
				  (cond ((null (sync self)) (* +beat+ (pulse self) (bo self 1)))
					((or (listp (sync self)) (sync>pulse self)) (* (cdr (sync self)) (bo self 1)))
					(t (error "See pulse cond in your routine.")))))
			    (loop for ip in (dispatch-udp-list (udp-list self)) 
				  do (send-udp
				      (read-from-string
				       (format nil "(\"/~S\" ~{\"~S\"~})"
					       (read-from-string (remove #\/ (string (tag self))))
					       (append (bo self) (bo self :xpos) (list pulse)))) 
				      (car ip) (cadr ip)))
			    (when (buffer-out self) (setf (buffer-out self) (butlast (buffer-out self))))
			    (setf (offset self) (+ (/ pulse *latency*) +clock+)
				  (dyn-buffer self) (cdr (dyn-buffer self)))))
			(sleep *latency*))))

;------------------------------------------------------------------
;                                                              PLAY   

(defgeneric kill-routine (self))
(defgeneric act-routine (self))

(defmethod kill-routine ((self sequencing))
  (when (_threadp (gethash 'routine (mem-cache self)))
    (_kill-thread (gethash 'routine (mem-cache self)))
    (remhash 'routine (mem-cache self))
    (loop for ip in (dispatch-udp-list (udp-list self)) 
	  do (send-udp
	      (read-from-string
	       (format nil "(\"/~S\" \"0\")"
		       (read-from-string (remove #\/ (string (tag self)))))) 
	      (car ip) (cadr ip))))
  (setf (outset self) t
	(counter self) 0))

(defmethod act-routine ((self sequencing))
  (kill-routine self)
  (when (_thread-zombie (gethash 'compute (mem-cache self)))
    (setf (gethash 'compute (mem-cache self)) (_make-thread (routine self) self)))
  (if (functionp (gethash 'routine-initform (mem-cache self))) (setf (gethash 'routine (mem-cache self)) (_make-thread (gethash 'routine-initform (mem-cache self)) self)) (error "No assigned function! use > (set-routine &progn funcs)")) 
(format t "~S is playing on:~&" (name self))
  (dispatch-udp-list (udp-list self) :verbose t)
  (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag self))))))

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
	       (cond ((hash-table-p val) (format stream " :~S (MAKE-HASH-TABLE :TEST #'EQUALP)" s))
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
	(format stream " (DEFVAR ~S (CAR *ALL-SEQUENCING*))" (dub self))
	(format stream " (SETF (GETHASH 'COMPUTE (MEM-CACHE ~S)) (_MAKE-THREAD (ROUTINE ~S) ~S))" (dub self) (dub self) (dub self))))
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
	    (setf (dyn-buffer self) (list (next-event-probability nil net :result :eval :remanence (remanence self) :compute (odds self)))))
    (when (> (length (dyn-buffer self)) (buffer-size self))
      (push (car (last (dyn-buffer self))) (buffer-out self))
      (setf (dyn-buffer self) (cdr (dyn-buffer self)))))
      
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
