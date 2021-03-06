;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                 global path in N3

(defgeneric expand-path (in)
  (:method ((in string))
    (if (string= "~" (subseq in 0 1)) (concatenate 'string (directory-namestring (user-homedir-pathname)) (subseq in 2)) in))
  (:method  ((in pathname))
    (pathname (expand-path (namestring in)))))

(defvar *NEUROMUSE3-DIRECTORY* (namestring (asdf:component-pathname (asdf:find-system "n3"))))

(defvar *N3-BACKUP-DIRECTORY* (concatenate 'string (expand-path "~/Documents/") "neuromuse3-backup-networks/"))
(ensure-directories-exist *N3-BACKUP-DIRECTORY*)

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defvar *gnuplot* nil)
(cond ((probe-file (pathname "/usr/bin/gnuplot")) (setf *gnuplot* "/usr/bin/gnuplot"))
      ((probe-file (pathname "/opt/local/bin/gnuplot")) (setf *gnuplot* "/opt/local/bin/gnuplot"))
      (t (warn "GNUPLOT is required to display graph.")))
(defvar *display* nil)
(cond ((probe-file (pathname "/usr/bin/display")) (setf *display* "/usr/bin/display"))
      ((probe-file (pathname "/usr/bin/open")) (setf *display* "/usr/bin/open"))
      (t (warn "Display command line not listed.")))
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar *latency* 0.01)

;------------------------------------------------------------------
;                                                  mapping function

;; two arguments (som_name number_of_neurons) &key about topology

(defun flatten (lst)
  (if (endp lst)
      lst
      (if (atom (car lst))
	  (append (list (car lst)) (flatten (cdr lst)))
	  (append (flatten (car lst)) (flatten (cdr lst))))))

(defun repeat-lst (lst n)
   (let (s)
     (dotimes (e n s) (push lst s)) (flatten (reverse s))))

(defun dupli (list n)
    (if (eql list nil)
        nil
    (append (make-list n :initial-element (car list)) (dupli (cdr list) n))))

;; multidimensional map quadrare
(defgeneric quadrare (self nbre-neurons &key about topology))
(defmethod quadrare ((self som) (nbre-neurons integer) &key about topology)
  (let* ((y '())
	 (after-comma (if (and (integerp about) (>= about 0)) about 0))
	 (n-dim (if topology topology 2))
         (a (floor (expt nbre-neurons (/ 1 n-dim))))
	 (rt (if (field self) (if (listp (field self)) (car (field self)) (field self)) a))
         (r (dotimes (e a y) (push (let ((nc (/ (round (* (expt 10 after-comma) (* (/ rt a) e))) (expt 10 after-comma)))) (if (= nc (round nc)) (round nc) (float nc))) y)))
         (s))
    (setf (field self) rt
	  (topology self) n-dim)
    (dotimes (i n-dim s) (push (repeat-lst (dupli (reverse r) (expt a i)) (expt a (- (1- n-dim) i))) s))
    (let ((tmp (apply #'mapcar #'list s)))
      (if (= (length tmp) (length (remove-duplicates tmp :test #'equalp))) tmp (quadrare self nbre-neurons :about (1+ after-comma) :topology n-dim)))))

;;  multidimentionnelle random map
(defgeneric rnd-map (self nbre-neurons &key about topology))
(defmethod rnd-map ((self som) (nbre-neurons integer) &key about topology)
  (let* ((n-dim (if topology topology 2))
	 (field (cond
		  ((numberp (field self)) (loop repeat n-dim collect (field self)))
		  ((and (listp (field self)) (= (length (field self)) n-dim)) (field self))
		  (t (loop repeat n-dim collect 10))))
	 (after-comma (if (and (integerp about) (>= about 0)) about 0))
	 (s '()))
    (setf (field self) (if (loop for i in field always (= i (car field))) (car field) field)
	  (topology self) n-dim)
    (loop for i from 1 to nbre-neurons
       do (push (let (r) (dotimes (e n-dim r) (push (let ((rnd (/ (random (* (expt 10 after-comma) (nth e field))) (expt 10 after-comma)))) (if (= rnd (round rnd)) (round rnd) (float rnd))) r))) s))
    (if (= (length s) (length (remove-duplicates s :test #'equalp))) s (rnd-map self nbre-neurons :about (1+ after-comma) :topology n-dim))))

;------------------------------------------------------------------
;                                                proximity function

(defgeneric euclidean (arg1 arg2 &key modulo position weight)
  (:documentation "two arguments (arg_1, arg_2) &key (modulo[t/nil] position[t/nil] weight[number/list])"))

(defmethod euclidean ((arg1 neuron) (arg2 neuron) &key modulo position weight)
  (let ((n1 (if position (xpos arg1) (output arg1)))
	(n2 (if position (xpos arg2) (output arg2)))
	(net (id (net arg1))))
    (if modulo (sqrt (apply #'+ (mapcar #'(lambda (x y w) (* w (expt (- x (caar (ordinate (loop for i in (list y (+ y (field net)) (- y (field net))) collect (list i (abs (- x i)))) #'< :key #'cadr))) 2))) n1 n2 (complist weight n1))))
	(sqrt (apply #'+ (mapcar #'(lambda (x y w) (* w (expt (- x y) 2))) n1 n2 (complist weight n1)))))))

(defmethod euclidean ((arg1 som) (arg2 neuron) &key modulo position weight)
  ;; i.e. SOM activation
  (declare (ignore modulo position))
  (sqrt (apply #'+ (mapcar #'(lambda (x y w) (* w (expt (- x y) 2))) (input arg1) (output arg2) (complist weight (input arg1))))))

(defmethod euclidean ((arg1 list) (arg2 list) &key modulo position weight)
  (cond ((ignore-errors (and (loop for n in arg1 always (neuron-p (id n))) (loop for m in arg2 always (neuron-p (id m)))))
	 (let ((ocd (ordered-combinatorial-distribution (list arg1 arg2))))
	   (loop for i in ocd collect (cons (euclidean (id (car i)) (id (cadr i)) :modulo modulo :position position :weight weight) i))))
	((and (loop for n in arg1 always (numberp n)) (loop for n in arg2 always (numberp n)))
	 (if (numberp modulo) (sqrt (apply #'+ (mapcar #'(lambda (x y w) (* w (expt (- x (caar (ordinate (loop for i in (list y (+ y modulo) (- y modulo)) collect (list i (abs (- x i)))) #'< :key #'cadr))) 2))) arg1 arg2 (complist weight arg1))))
	     (sqrt (apply #'+ (mapcar #'(lambda (x y w) (* w (expt (- x y) 2))) arg1 arg2 (complist weight arg1))))))
	(t nil)))

(defmethod euclidean ((arg1 neuron) (arg2 list) &key modulo position weight) 
  (euclidean (list arg1) arg2 :modulo modulo :position position :weight weight))

(defmethod euclidean ((arg1 list) (arg2 neuron) &key modulo position weight)
  (euclidean arg1 (list arg2) :modulo modulo :position position :weight weight))

(defmethod euclidean ((arg1 list) (arg2 null) &key modulo position weight)
  ;; matrix of arg1
  (cond ((ignore-errors (loop for n in (flatten arg1) always (neuron-p (id n))))
	 (let ((r (list)))
	   (dotimes (i (length arg1) (nreverse r))
	     (loop for j from (1+ i) to (1- (length arg1))
		do
		  (let ((ar1 (if (listp (nth i arg1)) (nth i arg1) (id (nth i arg1))))
			(ar2 (if (listp (nth j arg1)) (nth j arg1) (id (nth j arg1)))))
		    (push (list (euclidean ar1 ar2 :modulo modulo :position position :weight weight) ar1 ar2) r))))))
	((apply #'= (mapcar #'length arg1))
	 (let ((r (list)))
	   (dotimes (i (length arg1) (nreverse r))
	     (loop for j from (1+ i) to (1- (length arg1))
		do (push (list (euclidean (nth i arg1) (nth j arg1) :modulo modulo :position position :weight weight) i j) r)))))
	(t nil)))

(defmethod euclidean ((arg1 number) (arg2 number) &key modulo position weight)
  (declare (ignore position weight))
  (if (numberp modulo) (mod (abs (- arg1 arg2)) modulo) (abs (- arg1 arg2))))

(defun hamming (n1 n2)
  (count t
	 (append
	  (map
	   'list
	   (lambda (x y) (if (eq x y) nil t)) n1 n2)				       
	  (nthcdr (length n1) n2)
	  (nthcdr (length n2) n1))))

;------------------------------------------------------------------
;                                             neighborhood function   

;; three arguments (distance, radius, learning-rate) ...

(defun gauss (x radius lr)
  (* lr (exp (/ (- (expt x 2)) (expt (/ radius 2) 2)))))

;; << mexican hat >>
(defun fn-mex (x radius lr &key inh) 
  (let ((h (if inh inh 1))
        (rad (/ (expt radius 2) lr)))
    (* (- lr (/ (expt x 2) rad)) 
       (exp (/ (/ (* -1 (expt x 2)) rad)
               (* h lr))))))

;------------------------------------------------------------------
;                                               decreasing function        

;; three arguments (epoch, initial-value, learning-time) ...

(defun exp-decay (epoch initial-value learning-time &optional (final-value 0.01))
  "The final value has to be different of the initial value (to avoid division by zero). 
When the final value is superior to initial value, the function becomes increasing."
  (* initial-value (exp (/ epoch (/ learning-time (log (/ final-value initial-value)))))))

;------------------------------------------------------------------
;                      compute function (-> next-event-probability)        

;; one argument (probability-list) ...
#|
The problist has to be well-formed:
(
 (item1 prob1)
 (item2 prob2)
 ...
 )
with sum of weight(i) = 1.0
|#
;(assert (= 1 (loop for i in (mapcar #'cadr problist) sum i)))

(defun rnd-weighted (problist &optional (r '(0)))
  (loop for i in (mapcar #'cadr problist) do (push (+ (car r) i) r))
  (let ((res (nth (1- (length (loop for i in (reverse r) while (> (- 1 (random 1.0)) i) collect i))) problist)))
    (values (car res) (cadr res))))

(defun max-weighted (problist &key head rep)
  (let* ((lst (ordinate problist #'> :key #'cadr))
	 (al (if (and rep head (eq (car (last head)) (cadr lst))) (cdr lst) lst))
	 (tmp (cons (car al) (loop for i in (cdr al) until (< (cadr i) (cadr (car al))) collect i)))
	 (res (nth (random (length tmp)) tmp)))
    (values (car res) (cadr res))))

;------------------------------------------------------------------
;                                                     miscellaneous

(defun >thrifty-code (a digit &optional (key :left) (from 0)) ;; possible key are -> :left (default), :right, :mid (implies an odd digit number)
  (case key
    (:right (reverse (replace-a 1 (- a from) (make-list digit :initial-element 0))))
    (:left (replace-a 1 (- a from) (make-list digit :initial-element 0)))
    ;; in the case of :mid, the value of from=0 -> clip the exceeding values and from=1 (default) -> apply modulo +/-(floor (/ digit 2))
    (:mid (when (oddp digit) (replace-a 1 (+ (cond
					       ((and (zerop from) (> a (floor (/ digit 2)))) (floor (/ digit 2)))
					       ((and (zerop from) (< a (* -1 (floor (/ digit 2))))) (* -1 (floor (/ digit 2))))
					       ((and (= 1 from) (> a (floor (/ digit 2)))) (mod a (floor (/ digit 2))))
					       ((and (= 1 from) (< a (* -1 (floor (/ digit 2))))) (mod a (* -1 (floor (/ digit 2)))))
					       (t a)) (floor (/ digit 2))) (make-list digit :initial-element 0))))))

(defun winner-take-all (lst &optional (ind 0) tmp) ;; the result is the position of the 'winner' in lst
  (let ((wta (loop for x in (if tmp tmp lst) for i from 0 when (= x (reduce #'max (if tmp tmp lst))) collect i))) 
       (if (singleton wta)
	   (car wta)
	   (if (< ind (length lst))
	       (winner-take-all
		lst
		(1+ ind)
		(loop for x in (butlast (loop for i from 0 to (length lst) collect (subseq lst (if (< (- i ind) 0) 0 (- i ind)) (if (> (+ ind i 1) (length lst)) (length lst) (+ ind i 1))))) for pos from 0 collect (if (member pos wta) (reduce #'+ x) 0)))
	       (nth (random (length wta)) wta)))))

(defun <thrifty-code (lst &optional (key :left) (from 0))
  (case key
    (:right (+ (1- from) (- (length lst) (winner-take-all lst))))
    (:left (+ (winner-take-all lst) from))
    (:mid (- (winner-take-all lst) (floor (/ (length lst) 2))))))

;------------------------------------------------------------------
