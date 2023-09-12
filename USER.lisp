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
(ensure-directories-exist (format nil "~Adata/" *N3-BACKUP-DIRECTORY*))

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
;; [TODO] quadrare taking into account the field as list -- for now it takes only the first value ...
(defgeneric quadrare (self nbre-neurons &key about topology))
(defmethod quadrare ((self som) (nbre-neurons integer) &key about topology)
  (let* ((y '())
	 (after-comma (if (and (integerp about) (>= about 0)) about 0))
	 (n-dim (if topology topology (topology self)))
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
  (let* ((n-dim (if topology topology (topology self)))
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
;                                                   pick-experiment					
;                 as computing function (-> next-event-probability)        

;; ---> N3D ++++++++++++++++++++
;; (require 'N3D)
(defvar *remanence* 3)
(defun rnd-item (lst) (nth (random (length lst)) lst))
(defmethod pick-experiment ((self list)) (rnd-item self))
(defmethod pick-other-experience ((self list) (exp list))
  (let ((tmp (pick-experiment self)))
    (loop until (not (loop for i in tmp for j in exp always (if (eq '? j) t (= i j)))) do (setf tmp (pick-experiment self)))
    tmp))
;;++++++++++++++++++++++++++++++
;; /!\ mind there are additional keys (type and exclude)
;; one argument (probability-list)
;; key type (concern only self as problist when is set) ---> :random, :weighted, :inverse-weighted, max-weighted, min-weighted
;; key exclude ; exclude is a list of item(s)
;; add key include ; include is a list of item(s)
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

#|
;; to debug 
(defmethod pick-experiment ((self list) &key (type :weighted) exclude)
    (let (nproblist (r '(0)))
      (if exclude
	  (let ((npl (loop for i in self unless (member (car i) exclude :test #'equalp) collect i)))
	    (setf nproblist (mapcar #'list (mapcar #'car npl) (normalize-sum (mapcar #'cadr npl)))))
	  (setf nproblist self))      
      (case type
	(:random ; or :rnd
	 (values (car (nth (random (length nproblist)) nproblist)) (/ 1 (length nproblist))))
	(:weighted ; or :w
	 (loop for i in (mapcar #'cadr nproblist) do (push (+ (car r) i) r))
	 (let ((res (nth (1- (length (loop for i in (reverse r) while (> (- 1 (random 1.0)) i) collect i))) nproblist)))
	   (values (car res) (cadr res))))
	(:inverse-weighted ; or :iw
	 (loop for i in (normalize-sum (loop for i in (mapcar #'cadr nproblist) collect (- 1.0 i))) do (push (+ (car r) i) r))
	 (let ((res (nth (1- (length (loop for i in (reverse r) while (> (- 1 (random 1.0)) i) collect i))) nproblist)))
	   (values (car res) (cadr res))))
	(:max-weighted ; or :max
	 (when nproblist
	   (let* ((al (ordinate (mat-trans (list (car (mat-trans nproblist)) (cadr (mat-trans nproblist)))) #'> :key #'cadr))
		  (tmp (cons (car al) (loop for i in (cdr al) until (< (cadr i) (cadr (car al))) collect i)))
		  (res (nth (random (length tmp)) tmp)))
	     (values (car res) (cadr res)))))
	(:min-weighted ; or :min
	 (when nproblist
	   (let* ((al (ordinate (mat-trans (list (car (mat-trans nproblist)) (cadr (mat-trans nproblist)))) #'< :key #'cadr))
		  (tmp (cons (car al) (loop for i in (cdr al) until (> (cadr i) (cadr (car al))) collect i)))
		  (res (nth (random (length tmp)) tmp))) 
	     (values (car res) (cadr res)))))
	(otherwise
	 (let ((res (rnd-item self)))
	   (values res (/ 1 (length self))))))))

(defmethod pick-other-experience ((self list) (exp list) &key type exclude)
  ;; [TODO] output (values result prob)
  (let ((tmp (pick-experiment self :type type :exclude exclude)))
    ;; check if there is at least one possible result
    (loop until (not (loop for i in tmp for j in exp always (if (eq '? j) t (= i j)))) do (setf tmp (pick-experiment self)))
    tmp))
;;++++++++++++++++++++++++++++++

;; hack until rnd-weighted is replaced by pick-experiment in N3
(defun rnd-weighted (problist &key exclude)
(pick-experiment problist :type :weighted :exclude exclude))
|#

(defun drop-element (e set)
  (cond ((null set) '())
	((equal e (first set)) (rest set))
	(t (cons (first set) (drop-element e (rest set))))))   
                      
(defun complementary (subset set &optional kw)
  (if (eq kw :set)
      (loop for i in set unless (member i subset :test #'equalp) collect i)
      (cond ((null subset) set)
	    ((member (first subset) set)
	     (complementary (rest subset) (drop-element (first subset) set)))
	    (t (complementary (rest subset) set)))))

(defun rnd-equal (problist &key exclude)
  (let ((x (complementary exclude (mapcar #'car problist) :set)))
    (values (nth (random (length x)) x) (/ 1 (length x)))))

(defun rnd-weighted (problist &key exclude)
  (let (nproblist (r '(0)))
    (if exclude
	(let ((npl (loop for i in problist unless (member (car i) exclude :test #'equalp) collect i)))
	  (setf nproblist (mapcar #'list (mapcar #'car npl) (normalize-sum (mapcar #'cadr npl)))))
	(setf nproblist problist))
    (loop for i in (mapcar #'cadr nproblist) do (push (+ (car r) i) r))
    (let ((res (nth (1- (length (loop for i in (reverse r) while (> (- 1 (random 1.0)) i) collect i))) nproblist)))
      (values (car res) (cadr res)))))

(defun inverse-weighted (problist &key exclude)
  (let (nproblist (r '(0)))
    (if exclude
	(let ((npl (loop for i in problist unless (member (car i) exclude :test #'equalp) collect i)))
	  (setf nproblist (mapcar #'list (mapcar #'car npl) (normalize-sum (mapcar #'cadr npl)))))
	(setf nproblist problist))
    (loop for i in (normalize-sum (loop for i in (mapcar #'cadr nproblist) collect (- 1.0 i))) do (push (+ (car r) i) r))
    (let ((res (nth (1- (length (loop for i in (reverse r) while (> (- 1 (random 1.0)) i) collect i))) nproblist)))
      (values (car res) (cadr res)))))

(defun max-weighted (problist &key exclude)
  (let ((nproblist (remove nil (if exclude (loop for it in problist collect (when (not (member (car it) exclude :test #'equalp)) it)) problist))))
    (if nproblist
      (let* ((al (ordinate (mat-trans (list (car (mat-trans nproblist)) (normalize-sum (cadr (mat-trans nproblist))))) #'> :key #'cadr))
	     (tmp (cons (car al) (loop for i in (cdr al) until (< (cadr i) (cadr (car al))) collect i)))
	     (res (nth (random (length tmp)) tmp)))
	(values (car res) (cadr res))))))

(defun min-weighted (problist &key exclude)
  (let ((nproblist (remove nil (if exclude (loop for it in problist collect (when (not (member (car it) exclude :test #'equalp)) it)) problist))))
    (if nproblist
      (let* ((al (ordinate (mat-trans (list (car (mat-trans nproblist)) (normalize-sum (cadr (mat-trans nproblist))))) #'< :key #'cadr))
	     (tmp (cons (car al) (loop for i in (cdr al) until (> (cadr i) (cadr (car al))) collect i)))
	     (res (nth (random (length tmp)) tmp)))
	(values (car res) (cadr res))))))

;------------------------------------------------------------------
