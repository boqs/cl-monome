;;;; package.lisp

(defpackage :monome-serialraw
  (:use #:cl #:cffi #:optima)
  (:export #:*monome-devices*
	   #:with-monome-output #:with-monome-input
	   #:monome-button-event #:monome-button-press #:monome-button-release
	   #:x #:y
	   #:focus #:monome-focus-event
	   #:monome-receive-message
	   #:monome-request-device-info #:monome-request-id-string
	   #:monome-request-grid-size
	   #:monome-set-led #:monome-set-all
	   #:monome-map-leds #:monome-set-row #:monome-set-col
	   #:monome-set-intensity

	   #:monome-set-led-intensity #:monome-set-all-intensity
	   #:monome-map-intensities #:monome-row-intensities
	   #:monome-col-intensities

	   #:monome-map-128 #:setup-monome-dev #:grab-focus))

(defpackage :monome-serialosc
  (:use #:cl #:cffi #:optima #:usocket #:osc)
  (:export #:*monome-devices*
	   #:with-monome-output #:with-monome-input
	   #:monome-button-event #:monome-button-press #:monome-button-release
	   #:x #:y
	   #:focus #:monome-focus-event
	   #:monome-receive-message
	   #:monome-request-device-info #:monome-request-id-string
	   #:monome-request-grid-size
	   #:monome-set-led #:monome-set-all
	   #:monome-map-leds #:monome-set-row #:monome-set-col
	   #:monome-set-intensity

	   #:monome-set-led-intensity #:monome-set-all-intensity
	   #:monome-map-intensities #:monome-row-intensities
	   #:monome-col-intensities

	   #:monome-map-128 #:setup-monome-dev #:grab-focus))
