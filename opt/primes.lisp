;; NEUROMUSE3
;;------------------------------------------------------------------

(in-package :N3)

;------------------------------------------------------------------
;                                                            PRIMES   

;https://sinistercode.com/public/donnie/blog/java-vs-lisp-prime-factors

(defparameter *prime-test-n* 74805770726454941)

(defun is-prime (n)
  (declare (fixnum n))
  (cond ((< n 4) (or (= n 2) (= n 3)))
        ((or (zerop (mod n 2)) (zerop (mod n 3))) nil)
          (t (loop for a of-type fixnum from 5 to (floor (sqrt n)) by 6
                never (or (zerop (mod n a)) (zerop (mod n (+ a 2))))))))

(defun prime-factors (n)
  (declare (fixnum n))
  (if (or (is-prime n) (= n 1))
      (list n)
      (loop for a from 2 to (floor n 2)
         when (zerop (mod n a)) do
           (return (append (prime-factors a) (prime-factors (/ n a)))))))

#|
-------------------------------------------------------------
(lisp-implementation-type) > "Clozure Common Lisp"
(lisp-implementation-version) > "Version 1.12.1  DarwinX8664"
(time (prime-factors *prime-test-n*))

(PRIME-FACTORS *PRIME-TEST-N*)
took 3,461,628 microseconds (3.461628 seconds) to run.
During that period, and with 4 available CPU cores,
     3,454,382 microseconds (3.454382 seconds) were spent in user mode
         3,385 microseconds (0.003385 seconds) were spent in system mode
 144 bytes of memory allocated.
 0 minor page faults, 1 major page faults, 0 swaps.
(108413167 690006323)
-------------------------------------------------------------
(lisp-implementation-type) > "SBCL"
(lisp-implementation-version) > "2.1.10"
(time (prime-factors *prime-test-n*))

Evaluation took:
  1.298 seconds of real time
  1.298326 seconds of total run time (1.296789 user, 0.001537 system)
  100.00% CPU
  3,506,839,374 processor cycles
  0 bytes consed
  
(108413167 690006323)
-------------------------------------------------------------
|#
