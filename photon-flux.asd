;;;; -*- Mode: Lisp; fill-column: 90 -*-
;;;; photon-flux.asd

(asdf:defsystem #:photon-flux
  :description "Utility library for photon flux calculations"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license  "Permissive"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-annot #:lisp-unit #:codata-recommended-values)
  :components ((:file "physics-constants")
	       (:file "photon-calculations")))
