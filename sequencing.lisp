;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;; *latency* = 0.01 by default (-> file USER.lisp)
;; *remanence* = 3  by default (-> file USER.lisp)
;------------------------------------------------------------------
;                                                             CLOCK   

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
   (buffer-out
    :initform nil :initarg :buffer-out :accessor buffer-out :type list)
   (buffer-size
    :initform 30 :initarg :buffer-size :accessor buffer-size :type integer)
   (buffer-thres
    :initform 3 :initarg :buffer-thres :accessor buffer-thres :type integer)
   (pulse
    :initform 4 :initarg :pulse :accessor pulse :type number)
   (sync
    :initform nil :initarg :sync :accessor sync)
   (meter
    :initform 4 :initarg :meter :accessor meter :type number)
   (anacrusis
    :initform 0 :initarg :anacrusis :accessor anacrusis :type integer)
   (pattern
    :initform nil :initarg :pattern :accessor pattern :type list)
   (rule
    :initform nil :initarg :rule :accessor rule)
   (routine
    :initform nil :initarg :routine :accessor routine)
   (subroutine
    :initform nil :initarg :subroutine :accessor subroutine) ;; nil is silent 
   (ip
    :initform "127.0.0.1" :initarg :ip :accessor ip :type string)
   (port
    :initform 7771 :initarg :port :accessor port :type integer)
   (tag
    :initform '/N3 :initarg :tag :accessor tag)
   (mem-cache
    :initform (make-hash-table :test #'equalp) :initarg :mem-cache :accessor mem-cache)))

(defvar +routine+ nil)
(defvar +beat+ 1)

(defmethod initialize-instance :after ((self sequencing) &key name)
  (let ((ar (if name 
               (make-new-symbol name)
	       (make-new-symbol 'sequencing))))
    (setf (name self) ar
          (symbol-value ar) self
	  (gethash 'next-pulse (mem-cache self)) 0
	  (gethash 'routine-initform (mem-cache self)) +routine+)
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

(defun init-sequencing (&key dub description net buffer-size buffer-thres pulse sync meter anacrusis pattern tag ip port)
  (push (make-instance 'sequencing :name 'sequencing) *all-sequencing*)
  (let ((seq (car *all-sequencing*)))
    (setf (dub seq) (if dub (read-from-string (string dub)) (read-from-string (format nil "*untitled-~S*" (get-universal-time)))))
    (when (stringp description) (setf (description seq) description))
    (when (or (mlt-p (id net)) (area-p (id net))) (setf (net seq) (id net)))
    (when (integerp buffer-size) (setf (buffer-size seq) buffer-size))
    (when (integerp buffer-thres) (setf (buffer-thres seq) buffer-thres))
    (when (numberp pulse) (setf (pulse seq) pulse))
    (when (sequencing-p (id sync)) (setf (sync seq) (dub (id sync))))
    (when (numberp meter) (setf (meter seq) meter))
    (when (integerp anacrusis) (setf (anacrusis seq) anacrusis))
    (when pattern (setf (pattern seq) pattern))
    (when tag (setf (tag seq) (read-from-string (string tag))))
    (when (stringp ip) (setf (ip seq) ip))
    (when (integerp port) (setf (port seq) port))
    (eval (list 'defparameter (read-from-string (format nil "~S" (dub seq))) seq))))

(defun create-sequencing (&key dub description net buffer-size buffer-thres pulse sync meter anacrusis pattern tag ip port)
  (init-sequencing :dub dub :description description :net net :buffer-size buffer-size :buffer-thres buffer-thres :pulse pulse :sync sync :meter meter :anacrusis anacrusis :pattern pattern :tag tag :ip ip :port port))

;------------------------------------------------------------------
;                                                            THREAD   

#+openmcl(progn
	   ;(defun _initform-thread (thread) (car (ccl::process-initial-form thread)))
	   (defun _threadp (object) (typep object 'ccl:process)) 
	   (defun _all-threads () (ccl:all-processes))
	   (defun _make-thread (function name) (ccl:process-enable (ccl:process-preset (ccl:make-process name) function name)))
	   (defun _thread-zombie (thread) (ccl:process-exhausted-p thread))
	   (defun _kill-thread (thread) (ccl:process-kill thread)))

#+sbcl(progn
	   (defun _threadp (object) (sb-thread::thread-p object)) 
	   (defun _all-threads () (sb-thread::list-all-threads))
	   (defun _make-thread (function name) (sb-thread::make-thread (lambda () (funcall function name)) :name (string (name name))))
	   (defun _thread-zombie (thread) (and (sb-thread::thread-p thread) (not (sb-thread::thread-alive-p thread))))
	   (defun _kill-thread (thread) (sb-thread::terminate-thread thread)))

;------------------------------------------------------------------
;                                                    DEBUGGING TOOL   

;; *debug* = nil by default (-> file UTILS.lisp)

(defun check-thread (&optional sequencing only)
  (if sequencing
      (if (sequencing-p sequencing)
	  (let ((self sequencing))
	    (format t "~&;------------------------~&; ~S" (dub self))
	    ;;-------------
	    (cond ((and (_threadp (gethash 'compute (mem-cache self))) (_thread-zombie (gethash 'compute (mem-cache self))))
		   (format t "~&;compute -> thread zombie ~S" (gethash 'compute (mem-cache self))))
		  ((and (_threadp (gethash 'compute (mem-cache self))) (not (_thread-zombie (gethash 'compute (mem-cache self))))) (format t "~&;compute -> thread running (buffer-out is ~S) ~S" (if (>= (length (buffer-out self)) (buffer-size self)) 'filled 'filling...) (gethash 'compute (mem-cache self))))
		  (t
		   (format t "~&;compute -> thread NIL")))
	    ;;-------------	    
	    (cond ((and (_threadp (gethash 'routine (mem-cache self))) (_thread-zombie (gethash 'routine (mem-cache self))))
		   (format t "~&;routine -> thread zombie ~S" (gethash 'routine (mem-cache self))))
		  ((and (_threadp (gethash 'routine (mem-cache self))) (not (_thread-zombie (gethash 'routine (mem-cache self)))))
		   (format t "~&;routine -> thread running ~S" (gethash 'routine (mem-cache self))))
		  (t
		   (format t "~&;routine -> thread NIL")))	    
	    ;;-------------
	    (unless only
	      (when (sequencing-p (gethash 'subroutine (mem-cache self)))
		(format t "~&;SUBROUTINE")
		(check-thread (gethash 'subroutine (mem-cache self))))))
	  (error "~S is not a sequencing!" sequencing))
      (progn
	(cond ((and (_threadp ++clock++) (_thread-zombie ++clock++)) (format t ";clock -> thread zombie ~S" ++clock++))
	      ((and (_threadp ++clock++) (not (_thread-zombie ++clock++))) (format t ";clock -> thread running ~S" ++clock++))
	      (t (format t ";clock -> thread NIL")))	
	(loop for self in *all-sequencing*
	      do
		 (check-thread self t)))))

;------------------------------------------------------------------
;                                                               SET   

(defmethod test-clique ((self sequencing) (clique list) &key (as-nodes t)) ;; ---> as-nodes means as-learned
  (let ((net (id (net self))))
    (when (area-p net)
      (and
       (= (length (soms-list net)) (length clique))
       (loop for c in clique for i from 0 always (and (integerp c) (>= c 0) (< c (nth i (fanaux-length net)))))
       (if as-nodes (clique-p clique net) t)))))

(defmethod test-clique ((self list) (clique sequencing) &key (as-nodes t))
  ;; /!\ in this method the sequencing is the second argument and the first argument is a list of cliques
  (loop for i in self always (test-clique clique i :as-nodes as-nodes)))

(defgeneric clique>xpos (self clique)
  (:method ((self mlt) (clique list)) (clique>xpos (id (net self)) clique))
  (:method ((self area) (clique list))
    (when (test-clique self clique)
      (loop for i in clique for n in (soms-list self) collect (xpos (id (nth i (fanaux-list (id n)))))))))
  
(defgeneric bo (self &optional ind)
  (:documentation "return the clique to send, 
<ind> as interger from 0 to the length of the clique minus one returns the value at this <ind> position, 
<ind> as the keyword :xpos returns the position of the clique in the area,
<ind> as the keyword :next returns the clique to send + the next clique.
IMPORTANT NOTICE: keep in mind that the values of the clique must be understood as indices.")
  (:method ((self sequencing) &optional ind)
    (cond
      ;; ind integer
      ((integerp ind) (nth ind (carlast (buffer-out self))))
      ;; ind :xpos
      ((and (eq ind :xpos) (area-p (id (net self))))
       (clique>xpos (id (net self)) (carlast (buffer-out self))))
      ;; ind :next
      ((eq ind :next)
       (append (carlast (buffer-out self)) (cadr (reverse (buffer-out self)))))
      ;; else
      (t (carlast (buffer-out self))))))

(defmacro set-routine (self &body funcs)
  "Managing buffer-out according to the rule(s) in (rule self)... 
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     ;(unless (rule ,self) (warn "Set rule if required ...~&"))
     (progn
	   (when (_threadp (gethash 'routine (mem-cache ,self))) (_kill-thread (gethash 'routine (mem-cache ,self))) (remhash 'routine (mem-cache ,self)))
	   (when (_threadp (gethash 'compute (mem-cache ,self))) (_kill-thread (gethash 'compute (mem-cache ,self))) (remhash 'compute (mem-cache ,self)))
	   (setf (routine ,self)
		 (lambda* (self)
		   (loop do
		     (when (<= (length (buffer-out self)) (buffer-size self)) ,@funcs)
		     (sleep *latency*)))
		 (gethash 'compute (mem-cache ,self))
		 (_make-thread (routine ,self) ,self))
	   (format t "OSC will send message as /~A at ~A:~S~&" (read-from-string (remove #\/ (string (tag ,self)))) (ip ,self) (port ,self)))))

(setf +routine+
      (lambda* (self)
	       (loop do
		 (cond
		   ;;-------------------
		   ((and
		     (>= (length (buffer-out self)) (buffer-thres self))
		     (not (gethash 'subroutine-state (mem-cache self)))
		     (if (and (sync self) (not (gethash 'init-clock (mem-cache self))))
			 (let ((div (/ (* (pulse (id (sync self))) (meter self)) +beat+ *latency*)))
			   (= (mod (- div (/ (anacrusis self) +beat+ *latency*)) div)
			      (mod (- +clock+ (gethash 'init-clock (mem-cache (id (sync self))))) div)))
			 (>= +clock+ (gethash 'next-pulse (mem-cache self)))))
		    ;; SEND EVENT
		    (when *debug*
		      (format t "~S -> /~S 1 ~{~S ~}~&" (dub self)
			      (read-from-string (remove #\/ (string (tag self))))
			      (append (bo self (gethash 'ind (mem-cache self))) (list (/ (* 1.0 (1+ (bo self 0))) (pulse self) +beat+)))))
		    (when (and *debug* (sync self)) (format t "[~S]~&" +clock+))
		    (send-udp
		     (read-from-string
		      (format nil "(\"/~S\" \"1\" ~{\"~S\"~})"
			      (read-from-string (remove #\/ (string (tag self))))
			      (append (bo self (gethash 'ind (mem-cache self))) (list (/ (* 1.0 (1+ (bo self 0))) (pulse self) +beat+))))) 
		     (ip self) (port self))
		    ;; TO THIRDPART
		    (when (and *thirdpart* (gethash 'thirdpart (mem-cache self))) (mapcar #'funcall (gethash 'thirdpart (mem-cache self)) self))		    
		    ;; SET INIT-CLOCK
		    (unless (gethash 'init-clock (mem-cache self))
		      (setf
		       (gethash 'init-clock (mem-cache self)) +clock+
		       (gethash 'next-pulse (mem-cache self)) +clock+)
		      (when (and *debug* (not (sync self))) (format t "~S -> init [~S]~&" (dub self) +clock+)))
		    ;; SET NEXT-PULSE
		    (setf (gethash 'next-pulse (mem-cache self)) (+ (gethash 'next-pulse (mem-cache self)) (/ (* 1.0 (1+ (bo self 0))) (* (pulse self) +beat+ *latency*))))
		    ;; SET LAST-EVENT
		    (push (bo self) (gethash 'last-event (mem-cache self)))
		    (when (> (length (gethash 'last-event (mem-cache self))) *remanence*)
		      (setf (gethash 'last-event (mem-cache self)) (butlast (gethash 'last-event (mem-cache self)))))
		    ;; UPDATE BUFFER
		    (setf (buffer-out self) (butlast (buffer-out self))))
		   ;;-------------------
		   ((and (< (length (buffer-out self)) (buffer-thres self)) (not (gethash 'subroutine-state (mem-cache self))))
		    (setf (gethash 'subroutine-state (mem-cache self)) t)
		    (if (sequencing-p (id (subroutine self)))
			(act-routine (subroutine self))
			(progn
			  (when *debug* (format t "~S -> /~S 2 PAUSE~&" (dub self) (read-from-string (remove #\/ (string (tag self))))))
			  (send-udp (read-from-string (format nil "(\"/~S\" \"2\")" (read-from-string (remove #\/ (string (tag self)))))) (ip self) (port self)))))
		    ;;-------------------
		   ((and (gethash 'subroutine-state (mem-cache self)) (>= (length (buffer-out self)) (buffer-size self)))
		    (setf (gethash 'subroutine-state (mem-cache self)) nil
			  (gethash 'init-clock (mem-cache self)) nil)
		    (when (sequencing-p (id (subroutine self))) (kill-routine (subroutine self))))
		   ;;-------------------
		   ;; CLOCK THE ROUTINE
		   (t (sleep *latency*))))))

(defmacro set-rule (self &body funcs)
  "The last function have to return a partial clique or tournoi according to the involved net...
funcs take a sequencing class as argument -- conventionally named self"
  `(when (sequencing-p ,self)
     (setf (rule ,self) (lambda* (self) ,@funcs))
     (loop for rl in (nthcdr 2 (ml! (rule ,self))) for ind from 1 do (format t "#~S ~S~&" ind rl))))

(defgeneric set-pattern (self lst)
  (:method ((self sequencing) (lst list))
    (setf (pattern self) lst
	  (gethash 'pattern-counter (mem-cache self)) 0)))

(defgeneric set-sync (follower leader &key anacrusis save)
  (:method ((follower sequencing) (leader sequencing) &key anacrusis save)
    (setf (sync follower) (dub leader))
    (when anacrusis (setf (anacrusis follower) anacrusis))
    (when save (save follower))
    (let ((data (list
		 (list :leader (dub leader) (pulse leader) (meter leader) (sync leader) (anacrusis leader))
		 (list :follower (dub follower) (pulse follower) (meter follower) (sync follower) (anacrusis follower)))))
      (format-table t data
                    :column-label '("" "sequencing" "pulse" "meter" "sync" "anacrusis")
                    :column-align '(:left :left :right :right :left :right))))
  (:method ((follower sequencing) (leader null) &key anacrusis save)
    ;; hack to store sync
    (setf anacrusis (sync follower))
    (setf (sync follower) nil)
    (when save (save follower))
    (format t "~S as ~S is not sync~A with any other sequencing!" (dub follower) (name follower) (if anacrusis " anymore" ""))))

(defgeneric set-subroutine (self subroutine &key sync anacrusis)
  (:method ((self sequencing) (subroutine sequencing) &key sync anacrusis)
    (when (or sync (sync self)) (set-sync subroutine self :anacrusis (if anacrusis anacrusis (anacrusis subroutine))))
    (setf (subroutine self) subroutine)
    (let ((data (list
		 (list (if (sync subroutine) :leader :routine) (dub self) (pulse self) (meter self) (sync self) (when (sync self) (anacrusis self)))
		 (list (if (sync subroutine) :follower :subroutine) (dub subroutine) (pulse subroutine) (meter subroutine) (sync self) (when (sync subroutine) (anacrusis subroutine))))))
      (format-table t data
                    :column-label '("" "sequencing" "pulse" "meter" "sync" "anacrusis")
                    :column-align '(:left :left :right :right :left :right)))))
      
(defmethod group-list ((lst list) (seg t) &optional mode)
  (declare (ignore mode))
  (when (eq seg :as-prob) (let ((tmp (mat-trans (history lst)))) (mapcar #'reverse (mat-trans (list (car tmp) (mapcar #'float (normalize-sum (cadr tmp)))))))))

(defgeneric set-corpus (self corpus))

(defmethod set-corpus ((self sequencing) (corpus string))
  (let ((var (intern (format nil "+~A+" (remove #\* (string (dub self))))))
	(file (format nil "~Adata/~A.corpus" *N3-BACKUP-DIRECTORY* (remove #\* (string (dub self)))))
	(data (when (probe-file (pathname (expand-path (namestring corpus)))) (read-file (expand-path (namestring corpus))))))
    (if data
	(progn
	  (>data-file file data)
	  (eval `(defparameter ,var ',data))
	  (setf (net self) :corpus))
	(warn "~A does not exist or is empty!" file))))

(defmethod set-corpus ((self sequencing) (corpus pathname)) ;; specific path
  (set-corpus self (expand-path (namestring corpus))))

(defmethod set-corpus ((self sequencing) (corpus symbol)) ;; default path with default name
  (when (eq :load corpus)
    (set-corpus self (format nil "~Adata/~A.corpus" *N3-BACKUP-DIRECTORY* (remove #\* (string (dub self)))))))

(defmethod set-corpus ((self sequencing) (corpus list)) ;; data list /!\ this will overwrite the default corpus file
  (let ((file (format nil "~Adata/~A.corpus" *N3-BACKUP-DIRECTORY* (remove #\* (string (dub self))))))
    (>data-file file corpus))
  (set-corpus self :load))
	  
(defmethod set-corpus ((self sequencing) (corpus fn-with-code)) ;; from lambda* function with self as argument
  (set-corpus self (funcall corpus self)))

(defmethod set-corpus :after ((self sequencing) (corpus fn-with-code)) ;; store corpus function 
  (setf (gethash 'corpus (mem-cache self)) (ml! corpus)))

(defmethod next-event-probability ((head integer) (seq sequencing) &key result remanence compute opt)
  (when (eq :corpus remanence) (next-event-probability head (symbol-value (read-from-string (format nil "N3::+~A+" (remove #\* (string (dub seq)))))) :result result :remanence :corpus :compute compute :opt opt)))
  
;------------------------------------------------------------------
;                                                              PLAY   

(defgeneric kill-routine (self &key iskilled))
(defgeneric act-routine (self))

(defmethod kill-routine ((self sequencing) &key iskilled)
  (when (_threadp (gethash 'routine (mem-cache self)))
    (_kill-thread (gethash 'routine (mem-cache self)))
    (remhash 'routine (mem-cache self))
    (when *debug* (format t "~S -> /~S 0 FREE~&" (dub self) (read-from-string (remove #\/ (string (tag self))))))
    (send-udp (read-from-string (format nil "(\"/~S\" \"0\")" (read-from-string (remove #\/ (string (tag self)))))) (ip self) (port self))
    (setf iskilled t))
  (setf	(gethash 'next-pulse (mem-cache self)) 0)
  (remhash 'init-clock (mem-cache self))
  (remhash 'subroutine-state (mem-cache self))
  (when iskilled (format t "~S disconnected ...~&" (dub self))))

(defmethod act-routine ((self sequencing))
  (kill-routine self)
  (when (_thread-zombie (gethash 'compute (mem-cache self)))
    (setf (gethash 'compute (mem-cache self)) (_make-thread (routine self) self)))
  (setf (gethash 'routine (mem-cache self)) (_make-thread (gethash 'routine-initform (mem-cache self)) self))
  (format t "~S is playing on ~A:~S with TAG: /~A~&" (name self) (ip self) (port self) (read-from-string (remove #\/ (string (tag self)))))
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
			  ((listp (carlast mvl)) (format stream " :~S ~S" s (if (ml? val) (ml! val) val)))				
			  (t (format stream " :~S #'~S" s (carlast mvl))))))
		     ((or (eq s (read-from-string "NAME"))
			  (eq s (read-from-string "BUFFER-OUT")))
		      (format stream ""))
		     ((eq s (read-from-string "SYNC"))
		      (format stream " :~S ~S" s (when (sequencing-p (id val)) (dub (id val)))))
		     ((or (eq 'SYMBOL (type-of (id val)))
			  (eq 'SYMBOL (type-of val))
			  (eq s (read-from-string "DUB"))
			  (and (listp val) (not (null val))))
		      (format stream " :~S (QUOTE ~S)" s val))
		     (t (format stream " :~S ~S" s val)))))
	(format stream ") N3::*ALL-SEQUENCING*)")
	(format stream " (DEFVAR ~S (CAR *ALL-SEQUENCING*))" (dub self))
	(when (eq (net self) :corpus)
	  (if (gethash 'corpus (mem-cache self))
	    (format stream " (SET-CORPUS ~S ~S)" (dub self) (gethash 'corpus (mem-cache self)))  
	    (format stream " (SET-CORPUS ~S :LOAD)" (dub self))))
	;----------------
	(when (gethash 'thirdpart (mem-cache self))
	  (loop for fn in (gethash 'thirdpart (mem-cache self))
		do (when (ml? fn) (format stream "(PUSH ~S (GETHASH 'THIRDPART (MEM-CACHE ~S)))" (ml! (gethash 'processing (mem-cache self))) (dub self)))))
	;----------------
	(when (gethash 'ind (mem-cache self)) (format stream "(SETF (GETHASH 'IND (MEM-CACHE ~S)) ~S)" (dub self) (gethash 'ind (mem-cache self))))
	(when (pattern self) (format stream "(SETF (GETHASH 'PATTERN-COUNTER (MEM-CACHE ~S)) 0)" (dub self)))
	(when (routine self) (format stream " (SETF (GETHASH 'COMPUTE (MEM-CACHE ~S)) (_MAKE-THREAD (ROUTINE ~S) ~S))" (dub self) (dub self) (dub self)))))
    (UIOP:run-program (format nil "sh -c '~S ~S'" *UPDATE-SAVED-NET* path))))

;;; ;;    ;  ; ;  ;;; ;  ;;  ;;; ;;    ;

(defvar *SCAN-SEQ* (concatenate 'string *NEUROMUSE3-DIRECTORY* "bin/scan-seq"))

(defun scan-seq (file &optional res copy refname)
  "<scan-seq> collect all net identified by the keyword :NET in a sequencing file, and all global variable wrapped between asterisks, 
in order to warn if one of them is unbound."
  (unwind-protect 
       (if (open file :if-does-not-exist nil)
	   (let ((tn (pathname-type (pathname file)))) 
	     (cond ((equalp tn "seq")
		    (UIOP:run-program (format nil "sh -c '~S ~S ~S'" *SCAN-SEQ* file *N3-BACKUP-DIRECTORY*))
		    (setf refname (car (flatten (read-file (concatenate 'string *N3-BACKUP-DIRECTORY* ".refname")))))
		    (let ((var (remove-duplicates (loop for i in (flatten (read-file (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.var"))) unless (boundp i) collect i)))
			  (net (remove-duplicates (loop for i in (flatten (read-file (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.net"))) unless (boundp i) collect i)))
			  (pak (loop for p in (remove-duplicates (loop for i in (flatten (read-file (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.pak"))) unless (boundp i) collect i)) unless (find-package (read-from-string (string p))) collect p)))
		      (when var (push `(warn "Unbound variable(s) ~{~S ~}" ',var) res))
		      (when net (push `(warn "Unbound network(s) ~{~S ~}" ',net) res))
		      (when pak (push `(warn "Package~p ~{~a~#[~;, and ~:;, ~]~} required!" (length ',pak) ',pak) res))))))
	   (warn "This file does not exist.")))
  (UIOP:delete-file-if-exists (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.var"))
  (UIOP:delete-file-if-exists (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.net"))
  (UIOP:delete-file-if-exists (concatenate 'string *N3-BACKUP-DIRECTORY* ".tmp.pak"))
  (UIOP:delete-file-if-exists (concatenate 'string *N3-BACKUP-DIRECTORY* ".refname"))
  (if res
      (progn (mapcar #'eval res) nil)
      (if (boundp refname)
	  (format t "~A[~A] already loaded!~&" (name (id refname)) (dub (id refname)))
	  (progn
	    (load file)
	    (format t "~45<~A[~A] ...~;... ~A ...~>~%"
		    (name (car *all-sequencing*))
		    (dub (car *all-sequencing*))
		    (if copy "copied" "loaded"))))))

(defun load-sequencing (seq)
  (let ((file (if (zerop (length (directory-namestring (pathname (string seq)))))
		  (format nil "~A~A.seq" *N3-BACKUP-DIRECTORY* (string-upcase (string seq)))
		  (string seq))))
    (if (open file :if-does-not-exist nil)
	(scan-seq file)
	(warn "This file does not exist."))))

(defun sequencing-menu ()
  (let* ((stdout (format nil "~a" (UIOP:run-program (format nil "sh -c 'cd ~S; for f in *.seq; do echo $f; done'" *N3-BACKUP-DIRECTORY*) :output :string)))
	 (items (string-to-list stdout)))
    (when items
      (format t ";---------------")
      (dotimes (item-number (length items))
	(format t "~%~A: ~A" (+ item-number 1) (nth item-number items)))
      (format t "~%Load (Type any key to escape): ")
      (let ((val (read)))
	(if (listp val)
	    (loop for i in val do 
	      (when (and (integerp i) (<= i (length items)) (> i 0))
		(scan-seq (concatenate 'string *N3-BACKUP-DIRECTORY* (string (car (split-symbol (nth (- i 1) items)))) ".seq"))))
	    (when (and (integerp val) (<= val (length items)) (> val 0))
	      (scan-seq (concatenate 'string *N3-BACKUP-DIRECTORY* (string (car (split-symbol (nth (- val 1) items)))) ".seq"))))))))

(defun load-optional () ;(load (concatenate 'string *NEUROMUSE3-DIRECTORY* "opt/" (string filename) ".lisp"))
  (let* ((stdout (format nil "~a" (UIOP:run-program (format nil "sh -c 'cd ~S; for f in *.lisp; do echo $f; done'" (concatenate 'string *NEUROMUSE3-DIRECTORY* "opt/")) :output :string)))
	 (items (string-to-list stdout)))
    (when items
      (format t ";---------------")
      (dotimes (item-number (length items))
	(format t "~%~A: ~A" (+ item-number 1) (string (car (split-symbol (nth item-number items))))))
      (format t "~%Load (Type any key to escape): ")
      (let ((val (read)))
	(if (listp val)
	    (loop for i in val do 
	      (when (and (integerp i) (<= i (length items)) (> i 0))
		(load (concatenate 'string  *NEUROMUSE3-DIRECTORY* "opt/" (string (car (split-symbol (nth (- i 1) items)))) ".lisp"))))
	    (when (and (integerp val) (<= val (length items)) (> val 0))
	      (load (concatenate 'string  *NEUROMUSE3-DIRECTORY* "opt/" (string (car (split-symbol (nth (- val 1) items)))) ".lisp"))))))))

;------------------------------------------------------------------
;                                                           COMPUTE   

;; USER SPACE
;; routine function ...

(defgeneric markov-chain (self &key buffer odds)
  (:method ((self sequencing) &key buffer odds)
    (if (area-p (id (net self)))
	(let ((nc (if buffer
		      (next-event-probability (reverse (cons (funcall (rule self) self) buffer)) (id (net self)) :result :eval :remanence *remanence* :opt :buffer :compute odds)
		      ;; the first event does not depent of the rule ... [TODO] select from MLT onset ...
		      (next-event-probability nil (id (net self)) :result :eval :remanence *remanence* :compute odds))))
	  (if nc
	      (push nc (buffer-out self))
	      ;; if next-event-probability=nil remove oldest clique
	      (markov-chain self :buffer (butlast buffer) :odds odds)))
	(error "The net of ~S supposes to be an area.~&(net ~S) -> ~S" (name self) (dub self) (class-of (id (net self)))))))
	
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(make-instance 'neuron :name 'neuron)
(make-instance 'neuron :name 'ghost)
(make-instance 'area :name 'area)
(make-instance 'sequencing :name 'sequencing)
(defconstant MLT (make-instance 'mlt))
(format t "; MLT default functions:~&; DISTANCE-IN: ~S~&; DISTANCE-OUT: ~S~&; VOISINAGE: ~S~&; CARTE: ~S~&" (DISTANCE-IN MLT) (DISTANCE-OUT MLT) (VOISINAGE MLT) (CARTE MLT))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

