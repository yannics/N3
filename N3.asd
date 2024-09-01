
;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:asdf)

(defsystem N3
  :name "N3"
  :author "Yann Ics"
  :licence "Copyleft 2013/2024 - all wrongs reserved"
  :maintainer "<by.cmsc@gmail.com>"
  :description "Neuromuse3 - <https://github.com/yannics/Neuromuse3/blob/master/n3.pdf>"
  :version "3.0.25"
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
	       (:file "sequencing")
	       (:file "COPY")
	       )
  )
