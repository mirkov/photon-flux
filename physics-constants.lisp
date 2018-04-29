;;;; -*- Mode: Lisp; fill-column: 90 -*-
(cl:in-package :cl-user)

(defpackage :photon-flux.physics-constants
  (:use :cl)
  (:import-from :lisp-unit :define-test :assert-number-equal)
  (:documentation "Provides symbols physics constants in a more user friendly format"))


(cl-annot:enable-annot-syntax)

(in-package :photon-flux.physics-constants)

@export
(defconstant +h+ codata-recommended-values:planck-constant)

@export
(defconstant +c+ codata-recommended-values:speed-of-light-in-vacuum)

@export
(defconstant +q+ codata-recommended-values:elementary-charge)

@export
(defconstant +k-b+ codata-recommended-values:boltzmann-constant)

