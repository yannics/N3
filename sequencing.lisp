;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                             CLOCK   

(defvar +beat+ 1)
(defvar +clock+ 0)
#+sbcl(defvar ++clock++ (sb-thread:make-thread #'(lambda () (loop do (incf +clock+) (sleep *latency*))) :name "++clock++"))
#+openmcl(defvar ++clock++ (ccl:process-run-function "++clock++" #'(lambda () (loop do (incf +clock+) (sleep *latency*)))))

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
   (buffer-size
    :initform 30 :initarg :buffer-size :accessor buffer-size :type integer)
   (buffer-thres
    :initform 3 :initarg :buffer-thres :accessor buffer-thres :type integer)
   (pulse
    :initform 1 :initarg :pulse :accessor pulse :type number)
   (sync
    :initform nil :initarg :sync :accessor sync)
   (pattern
    :initform nil :initarg :pattern :accessor pattern :type list)
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
   (osc-in
    :initform nil :initarg :osc-in :accessor osc-in)
   (osc-out
    :initform nil :initarg :osc-out :accessor osc-out)
   (mem-cache
    :initform (make-hash-table :test #'equalp) :initarg :mem-cache :accessor mem-cache)))

(defvar +routine+ nil)

(defmethod initialize-instance :after ((self sequencing) &key name)
  (let ((ar (if name 
               (make-new-symbol name)
	       (make-new-symbol 'sequencing))))
    (setf (name self) ar
          (symbol-value ar) self)
    (setf
     (gethash 'outset (mem-cache self)) t
     (gethash 'beat-counter (mem-cache self)) 0
     (gethash 'routine-initform (mem-cache self)) +routine+)
    (set-subroutine self)
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

(defun init-sequencing (&key dub description net buffer-size buffer-thres pulse sync pattern tag meter remanence odds osc-in osc-out)
  (push (make-instance 'sequencing :name 'sequencing) *all-sequencing*)
  (let ((seq (car *all-sequencing*)))
    (setf (dub seq) (if dub dub (name seq)))
    (when description (setf (description seq) description))
    (when net (setf (net seq) net))
    (when buffer-size (setf (buffer-size seq) buffer-size))
    (when buffer-thres (setf (buffer-thres seq) buffer-thres))
    (when pulse (setf (pulse seq) pulse))
    (when sync (setf (sync seq) sync))
    (when pattern (setf (pattern seq) pattern))
    (when tag (setf (tag seq) tag))
    (when meter (setf (meter seq) meter))
    (when remanence (setf (remanence seq) remanence))
    (when odds (setf (odds seq) odds))
    (when osc-in (setf (osc-in seq) osc-in))
    (when osc-out (setf (osc-out seq) osc-out))
    (eval (list 'defparameter (read-from-string (format nil "~S" (dub seq))) seq))))

(defun create-sequencing (&key dub net description pulse pattern odds meter sync remanence buffer-size buffer-thres osc-in osc-out tag)
  (init-sequencing :dub dub :net net :description description :pulse pulse :pattern pattern :odds odds :meter meter :sync sync :remanence remanence :buffer-size buffer-size :buffer-thres buffer-thres :osc-in osc-in :osc-out osc-out :tag tag))

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

;------------------------------------------------------------------
;                                                   DEBUGGING TOOLS   

(defgeneric check-thread (self)
  (:method ((self sequencing))
    (cond ((and (_threadp ++clock++) (_thread-zombie ++clock++)) (format t ";clock -> thread zombie ~S" ++clock++))
	  ((and (_threadp ++clock++) (not (_thread-zombie ++clock++))) (format t ";clock -> thread running ~S" ++clock++))
	  (t (format t ";clock -> thread NIL")))
    (cond ((and (_threadp (gethash 'compute (mem-cache self))) (_thread-zombie (gethash 'compute (mem-cache self)))) (format t "~&;compute -> thread zombie ~S" (gethash 'compute (mem-cache self))))
	  ((and (_threadp (gethash 'compute (mem-cache self))) (not (_thread-zombie (gethash 'compute (mem-cache self))))) (format t "~&;compute -> thread running (buffer-out is ~S) ~S" (if (= (buffer-size self) (length (buffer-out self))) 'filled 'filling...) (gethash 'compute (mem-cache self))))
	  (t (format t "~&;compute -> thread NIL")))
    (cond ((and (_threadp (gethash 'routine (mem-cache self))) (_thread-zombie (gethash 'routine (mem-cache self)))) (format t "~&;routine -> thread zombie ~S" (gethash 'routine (mem-cache self))))
	  ((and (_threadp (gethash 'routine (mem-cache self))) (not (_thread-zombie (gethash 'routine (mem-cache self))))) (format t "~&;routine -> thread running ~S" (gethash 'routine (mem-cache self))))
	  (t (format t "~&;routine -> thread NIL")))
    (cond ((and (_threadp (gethash 'subroutine (mem-cache self))) (_thread-zombie (gethash 'subroutine (mem-cache self)))) (format t "~&;subroutine -> thread zombie ~S" (gethash 'subroutine (mem-cache self))))
	  ((and (_threadp (gethash 'subroutine (mem-cache self))) (not (_thread-zombie (gethash 'subroutine (mem-cache self))))) (format t "~&;subroutine -> thread running ~S" (gethash 'subroutine (mem-cache self))))
	  (t (format t "~&;subroutine -> thread NIL")))))

;------------------------------------------------------------------
;                                                               SET   

(defgeneric clique>xpos (self clique)
  (:method ((self mlt) (clique list)) (clique>xpos (id (net self)) clique))
  (:method ((self area) (clique list))
    (when (test-clique self clique)
      (loop for i in clique for n in (soms-list self) collect (xpos (id (nth i (fanaux-list (id n)))))))))

(defun dispatch-osc-out (osc-out &key verbose)
  (if verbose (format t "~{IP:~{~A~^:~}~&~}~&" (dispatch-osc-out osc-out))
      (progn (when (integerp (car (list! osc-out))) (setf osc-out (cons "127.0.0.1" (list! osc-out))))
	     (if osc-out
		 (let ((ind (loop for i in osc-out for pos from 0 when (stringp i) collect pos)))
		   (flat-once (mapcar #'(lambda (x) (reccomb (list (car x)) (list (cdr x))))
				      (loop for x in ind for next from 1
					 collect (subseq osc-out x (nth next ind))))))
		 (list (list "127.0.0.1" 7771))))))

(defgeneric bo (self &optional ind buf)
  (:documentation "return the clique to send, 
<ind> as interger from 1 to the length of the clique returns the value at this <ind> position, 
<ind> as the keyword :pos returns the position of the clique in the area.")
  (:method ((self sequencing) &optional ind buf)
    (cond
      ((and (pattern self) (eq (gethash 'subroutine-type (mem-cache self)) :pattern) (not (_threadp (gethash 'subroutine (mem-cache self)))) (gethash 'subroutine-state (mem-cache self)))
       (setf buf (nth (mod (gethash 'subroutine-counter (mem-cache self)) (length (pattern self))) (pattern self)))
       (incf (gethash 'subroutine-counter (mem-cache self)))) 
      ((and (eq (gethash 'subroutine-type (mem-cache self)) :pattern) (sequencing-p (gethash 'subroutine (mem-cache self))) (gethash 'subroutine-state (mem-cache self)))
       (setf buf (car (last (buffer-out (gethash 'subroutine (mem-cache self)))))))
      (t (setf buf (car (last (buffer-out self))))))
    (cond ((integerp ind) (nth (1- ind) buf))
	  ((and (eq ind :xpos) (or (area-p (id (net self))) (area-p (id (net (id (net self)))))))
	   (flatten (clique>xpos (id (net self)) buf)))
	  (t buf))))

;;------------------------------------
(defgeneric sync>pulse (self)
  (:method ((self sequencing))
    (setf (sync self)
	  (if (integerp (sync self))
	      (cons (sync self) (/ (* (meter self) +beat+) (sync self)))
	      (let ((val (1- (loop for i from 1 when (> (* i (pulse self)) (meter self)) return i))))
		(cons val (/ (* (meter self) +beat+) val)))))))

(defgeneric sync! (self)
  (:method ((self sequencing))
    (cond  ((and (gethash 'outset (mem-cache self)) (zerop (mod (/ +clock+ (* (pulse self) (/ +beat+ *latency*))) (meter self)))) (setf (gethash 'outset (mem-cache self)) nil) (incf (gethash 'beat-counter (mem-cache self))))
	   ((and (listp (sync self)) (integerp (rationalize (mod (/ +clock+ (* (cdr (sync self)) (/ +beat+ *latency*))) (meter self))))) (when (zerop (mod (/ +clock+ (* (pulse self) (/ +beat+ *latency*))) (meter self))) (incf (gethash 'beat-counter (mem-cache self)))) (>= +clock+ (gethash 'offset (mem-cache self)))))))			    ;;------------------------------------

(defmacro set-routine (self &body funcs)
  "Managing buffer-out according to the rule(s) in (rule self)... 
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
	   (dispatch-osc-out (osc-out ,self) :verbose t)
	   (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag ,self))))))
	 (warn "Set rule first..."))))

(defmacro set-rule (self &body funcs)
  "The last function have to return a partial clique or tournoi according to the involved net...
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     (setf (rule ,self) (lambda* (self) ,@funcs))))

(setf +routine+
      (lambda* (self)
	       (loop do (when (and (buffer-out self) (if (sync self) (sync! self) t))
			  (let ((pulse
				  (cond ((null (sync self)) (* +beat+ (pulse self) (bo self 1)))
					((or (listp (sync self)) (sync>pulse self)) (* (cdr (sync self)) (bo self 1)))
					(t (error "See pulse cond in your routine.")))))
			    (when (and (not (gethash 'subroutine-state (mem-cache self))) (= (length (buffer-out self)) (buffer-thres self))) (setf (gethash 'subroutine-state (mem-cache self)) t (pulse self) (gethash 'subroutine-pulse (mem-cache self))))	     
			    ;;------------------------------------
			    (cond
			      ((and (gethash 'subroutine-state (mem-cache self)) (member (gethash 'subroutine-type (mem-cache self)) '(:silent :rest)))
			       (loop for ip in (dispatch-osc-out (osc-out self)) 
				     do (send-udp (read-from-string (format nil "(\"/~S\" \"0\")" (read-from-string (remove #\/ (string (tag self)))))) (car ip) (cadr ip))))
			      ((and (gethash 'subroutine-state (mem-cache self)) (member (gethash 'subroutine-type (mem-cache self)) '(:sustain :pedal))) nil)
			      ((and (gethash 'subroutine-state (mem-cache self)) (sequencing-p (gethash 'subroutine (mem-cache self))) (not (_threadp (gethash 'routine (mem-cache (gethash 'subroutine (mem-cache self))))))) (act-routine (gethash 'subroutine (mem-cache self)))) ;; (TODO) add cond IF (gethash 'subroutine (mem-cache self)) is running THEN listen port of (gethash 'subroutine (mem-cache self)) + send with the tag of self
			      (t  (loop for ip in (dispatch-osc-out (osc-out self)) 
					do (send-udp
					    (read-from-string
					     (format nil "(\"/~S\" ~{\"~S\"~})"
						     (read-from-string (remove #\/ (string (tag self))))
						     (append (bo self) (bo self :xpos) (list pulse)))) 
					    (car ip) (cadr ip)))))
			    ;;------------------------------------
			    (unless (gethash 'subroutine-state (mem-cache self)) (setf (buffer-out self) (butlast (buffer-out self))))	     
			    (cond ((and (gethash 'subroutine-state (mem-cache self)) (member (gethash 'subroutine-type (mem-cache self)) '(:silent :rest :sustain :pedal :pattern)))
				   (loop until (and (not (gethash 'subroutine-lock (mem-cache self))) (= (length (buffer-out self)) (buffer-size self))) do (sleep *latency*))
				   (setf (gethash 'subroutine-state (mem-cache self)) nil))
				  (t (sleep pulse)))
			    (when (and (gethash 'subroutine-state (mem-cache self)) (= (length (buffer-out self)) (buffer-size self)))
			      (setf (gethash 'subroutine-state (mem-cache self)) nil (pulse self) (gethash 'main-pulse (mem-cache self)))
			      (when (sequencing-p (gethash 'subroutine (mem-cache self))) (kill-routine (gethash 'subroutine (mem-cache self))))))) ;; (TODO) IF listening port THEN stop sending 
			(sleep *latency*))))

 #|
subroutine attribute as key in (mem-cache self)
- subroutine ---> other sequencing as subroutine
- subroutine-pulse ---> pulse of the subroutine (pulse self) by default
- subroutine-type ---> (:silent :rest :sustain :pedal :pattern)
- subroutine-counter ---> nth through (pattern self)
- subroutine-state ---> switch to subroutine when it is true
- subroutine-lock ---> (TODO) according to the sync and/or until subroutine is completed
- subroutine-as-learned ---> keep track of the key :as-learned for save function
others
- main-pulse ---> keep track of the main pulse i.e. routine
|#

(defmethod test-clique ((self sequencing) (clique list) &key (as-nodes t)) ;; as-nodes means  as-learned
  (let ((net (id (net self))))
    (when (area-p net)
      (and
       (= (length (soms-list net)) (length clique))
       (loop for c in clique for i from 0 always (and (integerp c) (>= c 0) (< c (nth i (fanaux-length net)))))
       (if as-nodes (loop for c in clique always (clique-p c net)) t)))))

(defun set-subroutine (self &key (is :silent) pulse (with (pattern self)) (as-learned t))
  (when (and (sequencing-p self) (member is '(:silent :rest :sustain :pedal :pattern)))
    (setf
     (gethash 'subroutine-type (mem-cache self)) is
     (gethash 'subroutine-counter (mem-cache self)) 0
     (gethash 'subroutine-as-learned (mem-cache self)) as-learned
     (gethash 'subroutine-pulse (mem-cache self)) (if (numberp pulse) pulse (pulse self))
     (gethash 'main-pulse (mem-cache self)) (pulse self))
    (when (eq is :pattern)
      (cond ((sequencing-p (id with)) (setf (gethash 'subroutine (mem-cache self)) (id with)))
	    ((and (listp with) (test-clique self with :as-nodes as-learned)) (setf (pattern self) with))
	    ((and (listp with) (not (test-clique self with :as-nodes as-learned))) (warn "Invalid list of cliques ..."))
	    (with (warn "The keyword :with requires a sequencing as subroutine or a list of events as cliques ..."))))))
	  
;; (set-subroutine *voice1* :is :silent) ;; default value (:is optional if it is :silent or :rest)
;; (set-subroutine *voice1* :is :pattern :pulse 1) ;; it supposes (pattern self) as a list of cliques (:pulse optional only when it is different of the main pulse)
;; (set-subroutine *voice1* :is :pattern :with <list-of-cliques> :pulse 1 :as-learned nil) ;; (:pulse optional only when it is different of the main pulse, and :as-learned optional only if it is nil)
;; (set-subroutine *voice1* :is :pattern :with <sequencing> :pulse 1) ;; (:pulsed optional ...)

;------------------------------------------------------------------
;                                                              PLAY   

(defgeneric kill-routine (self &key message)) ;; message is a list of string to send via (osc-out self)
(defgeneric act-routine (self &key sync))

(defmethod kill-routine ((self sequencing) &key (message (read-from-string (format nil "(\"/~S\" \"0\")" (read-from-string (remove #\/ (string (tag self))))))))
  (when (_threadp (gethash 'routine (mem-cache self)))
    (_kill-thread (gethash 'routine (mem-cache self)))
    (remhash 'routine (mem-cache self))
    (when (and (listp message) (loop for i in message always (stringp i))) ;; (TODO) prepend tag ...
      (loop for ip in (dispatch-osc-out (osc-out self)) 
	    do (send-udp message (car ip) (cadr ip)))))
  (setf (gethash 'outset (mem-cache self)) t
	(gethash 'beat-counter (mem-cache self)) 0)
  nil)

(defmethod act-routine ((self sequencing) &key sync)
  (kill-routine self)
  (when sync (setf (sync self) t))
  (when (_thread-zombie (gethash 'compute (mem-cache self)))
    (setf (gethash 'compute (mem-cache self)) (_make-thread (routine self) self)))
  (if (functionp (gethash 'routine-initform (mem-cache self))) (setf (gethash 'routine (mem-cache self)) (_make-thread (gethash 'routine-initform (mem-cache self)) self)) (error "No assigned function! use > (set-routine &progn funcs)"))
  (format t "~S is playing on:~&" (name self))
  (dispatch-osc-out (osc-out self) :verbose t)
  (format t "TAG: /~A" (read-from-string (remove #\/ (string (tag self)))))
  (gethash 'routine (mem-cache self)))

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
			  (eq s (read-from-string "BUFFER-OUT")))
			  (format stream ""))
		     (t (format stream " :~S (QUOTE ~S)" s val)))))
	(format stream ") N3::*ALL-SEQUENCING*)")
	(format stream " (DEFVAR ~S (CAR *ALL-SEQUENCING*))" (dub self))
	(format stream " (SET-SUBROUTINE ~S :IS ~S :PULSE ~S :WITH ~S :AS-LEARNED ~S)" (dub self) (gethash 'subroutine-type (mem-cache self)) (gethash 'subroutine-pulse (mem-cache self)) (if (sequencing-p (gethash 'subroutine (mem-cache self))) (gethash 'subroutine (mem-cache self)) (pattern self)) (gethash 'subroutine-as-learned (mem-cache self)))
	(format stream " (SETF (GETHASH 'PATTERN (MEM-CACHE ~S)) ~S)" (dub self) (gethash 'pattern (mem-cache self)))
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

(defgeneric markov-chain (self &optional buffer)
  (:method ((self sequencing) &optional buffer)
    (unless (area-p (id (net self))) (error "The net of ~S supposes to be an area.~&(net ~S) -> ~S" (name self) (dub self) (class-of (id (net self)))))
    (let ((nc (if buffer
		  (next-event-probability (reverse (cons (funcall (rule self) self) buffer)) (id (net self)) :result :eval :remanence (remanence self) :opt :buffer :compute (odds self))
		  ;; the first event does not depent of the rule ...
		  (next-event-probability nil (id (net self)) :result :eval :remanence (remanence self) :compute (odds self)))))
      (unless nc
	;; if next-event-probability=nil remove oldest clique
	(markov-chain self (butlast buffer)))
      (push nc (buffer-out self)))))
    
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(make-instance 'neuron :name 'neuron)
(make-instance 'neuron :name 'ghost)
(make-instance 'area :name 'area)
(make-instance 'sequencing :name 'sequencing)
(defconstant MLT (make-instance 'mlt))
(format t "; MLT default functions:~&; DISTANCE-IN: ~S~&; DISTANCE-OUT: ~S~&; VOISINAGE: ~S~&; CARTE: ~S~&" (DISTANCE-IN MLT) (DISTANCE-OUT MLT) (VOISINAGE MLT) (CARTE MLT))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
