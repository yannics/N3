(progn
  (ignore-errors (require 'fosc))
  (unless (find-package 'fosc) (ignore-errors (require 'osc))))

(defpackage :N3
  (:use :cl)
  (:export :*N3-BACKUP-DIRECTORY* 
	   :*AVAILABLE-SOM*
	   :*AVAILABLE-AREA*
	   :CREATE-MLT
	   :CREATE-AREA
	   :LOCATE-CLIQUE
	   :LOCATE-TOURNOI
	   :UPDATE-FANAUX
	   :UPDATE-COVER-VALUE
	   :LEARN
	   :SAVE
	   :LOAD-NEURAL-NETWORK
	   :NEXT-EVENT-PROBABILITY
	  
	   :*TREE*
	   :GET-LEAVES
	   :CAH-FANAUX
	   :DENDROGRAM

	   :NET-MENU
	   :TREE-MENU
	   
	   :SEND-UDP
	   :OSC-LISTEN

	   :EUCLIDEAN
	   :EXP-DECAY
	   :FN-MEX
	   :GAUSS
	   :QUADRARE
	   :RND-MAP))

(in-package :N3)
(setf *random-state* (make-random-state t))
