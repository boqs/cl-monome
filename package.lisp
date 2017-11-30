;;;; package.lisp

(defpackage :monome-serialraw
  (:use #:cl #:cffi #:optima)
  (:export #:with-monome-output-stream #:with-monome-input-stream
	   #:monome-button-event #:monome-button-press #:monome-button-release
	   #:x #:y
	   #:monome-receive-message
	   #:monome-request-device-info #:monome-request-id-string
	   #:monome-request-grid-size
	   #:monome-set-led #:monome-set-all
	   #:monome-map-leds #:monome-set-row #:monome-set-col
	   #:monome-set-intensity

	   #:monome-set-led-intensity #:monome-set-all-intensity
	   #:monome-map-intensities #:monome-row-intensities
	   #:monome-col-intensities))

(defpackage :monome-serialosc
  (:use #:cl #:cffi #:optima)
  (:export #:with-monome-output-stream #:with-monome-input-stream
	   #:monome-button-event #:monome-button-press #:monome-button-release
	   #:x #:y
	   #:monome-receive-message
	   #:monome-request-device-info #:monome-request-id-string
	   #:monome-request-grid-size
	   #:monome-set-led #:monome-set-all
	   #:monome-map-leds #:monome-set-row #:monome-set-col
	   #:monome-set-intensity

	   #:monome-set-led-intensity #:monome-set-all-intensity
	   #:monome-map-intensities #:monome-row-intensities
	   #:monome-col-intensities))
