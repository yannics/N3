;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:asdf)

(defsystem N3
    :name "N3"
    :author "Yann Ics"
    :licence "Copyleft 2013/2020 - all wrongs reserved"
    :maintainer "<by.cmsc@gmail.com>"
    :description "Neuromuse3 - <https://www.overleaf.com/read/wswcpgqntjrc>"
    :version "3.0.13"
    :components ((:file "package")
      		  (:file "SOM")
		  (:file "MLT")
		  (:file "AREA")
		  (:file "USER")
		  (:file "SAVE")
		  (:file "UDP")		
                 (:file "UTILS")
		  (:file "CAH")
		  (:file "analysis")
		  (:file "sequencing")))