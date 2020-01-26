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
	  
	   :*TREE*
	   :GET-LEAVES
	   :CAH-FANAUX
	   :DENDROGRAM

	   :NET-MENU
	   :TREE-MENU
	   
	   :SEND-UDP
	   :OSC-LISTEN))
