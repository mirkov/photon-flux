;;;; -*- Mode: Lisp; fill-column: 90 -*-
(cl:in-package :cl-user)

(defpackage :photon-flux.photon-calculations
  (:use :cl)
  (:import-from :lisp-unit :define-test :assert-number-equal)
  (:documentation "Photon count and flux calculations")
  (:use :photon-flux.physics-constants))


(cl-annot:enable-annot-syntax)

(in-package :photon-flux.photon-calculations)

@export
(defun photon-energy (wavelength)
  (/ (* +h+ +c+) wavelength))

@export
(defun photon-rate (power wavelength)
  "Number of photons/second

Arguments and values:

POWER: beam power in Watt
WAVELENGTH: light wavelength in Meter"
  (let ((E-ph (photon-energy wavelength)))
    (/ power E-ph)))

@export
(defun photon-current (power wavelength)
  "Photon current

Arguments and values:

POWER: beam power in Watt
WAVELENGTH: light wavelength in Meter
PHOTON-CURRENT: Ampere

Notes: 

For verification, use \"Calculator for Photodiodes\" on https://www.rp-photonics.com/photodiodes.html
"
  (* +q+ (photon-rate power wavelength)))

@export
(defun photon-count (beam-energy wavelength)
  "Number of photons in a beam

Arguments and Values:

beam-energy: Beam energy in Joule, a real number
wavelength: Photon wavelength in Meter, a real number"
  (let ((E-ph (photon-energy wavelength)))
    (/ beam-energy E-ph)))

(define-test calculations
  "The calculations mimic the example at https://ecee.colorado.edu/~bart/book/ex010.htm"
  (assert-number-equal 2.48e-19 (photon-energy 800e-9))
  (photon-rate 1 800e-9))

(define-test photon-current
  (let* ((wavelength 900d-9)
	 (photon-current (photon-current 1 wavelength)))
    photon-current))

@export
(defun photon-energy/eV (wavelength)
  "Calculate photon energy as function of wavelength

Syntax:

photon-energy/eV wavelength -> energy/eV energy/Joule

Arguments and Values:

wavelength: the wavelength in meters, a real number
energy/eV: energy in units of eV, a real number
energy/Joule: energy in units of Joule, a real number"
  (let* ((J (/ (* +h+ +c+)
	       wavelength))
	 (eV (/ J +q+)))
    (values eV J)))

