;;;; cl-monome.lisp

(in-package #:cl-monome)

(defcenum monome-event-type-t
  :monome-button-up
  :monome-button-down
  :monome-encoder-delta
  :monome-encoder-key-up
  :monome-encoder-key-down
  :monome-tilt
  :monome-event-max)

(eval-when (:compile-toplevel :load-toplevel)
  (load-foreign-library "/usr/local/lib/libmonome.so"))

(defcfun "monome_open" :pointer
  (dev :string) (null :pointer))

(defcfun "monome_close" :pointer
  (monome :pointer))

(defcfun "monome_led_set" :int
  (x :uint) (y :uint) (on :uint))

(defcfun "monome_led_on" :int
  (monome :pointer) (x :uint) (y :uint))

(defcfun "monome_led_off" :int
  (monome :pointer) (x :uint) (y :uint))

(defcfun "monome_led_all" :int
  (monome :pointer) (status :uint))

(defcfun "monome_led_map" :int
  (monome :pointer) (x-off :uint) (y-off :uint) (data :pointer))

(defcfun "monome_led_col" :int
  (monome :pointer) (x :uint) (y-off :uint)  (count :uchar) (col-data :pointer))

(defcfun "monome_led_row" :int
  (monome :pointer) (x-off :uint) (y :uint)  (count :uchar) (row-data :pointer))

(defcfun "monome_led_intensity" :int
  (monome :pointer) (brightness :uint))

(defcfun "monome_led_level_set" :int
  (monome :pointer) (x :uint) (y :uint) (brightness :uint))

(defcfun "monome_led_level_all" :int
  (monome :pointer) (status :uint))

(defcfun "monome_led_level_map" :int
  (monome :pointer) (x-off :uint) (y-off :uint) (data :pointer))

(defcfun "monome_led_level_row" :int
  (monome :pointer) (x-off :uint)
  (y :uint) (count :uchar) (data :pointer))

(defcfun "monome_led_level_col" :int
  (monome :pointer) (x :uint) (y-off :uint)
  (count :uchar) (data :pointer))

(defcfun "monome_register_handler" :int
  (monome :pointer) (event-type :uint) (callback :pointer) (data :pointer))
;; this is  some glue code  added to  libmonome specially so  we don't
;; have to do struct comprehensions inside these bindings
(defcfun "monome_event_get_grid" :int
  (monome-event :pointer) (*x :pointer) (*y :pointer) (**monome :pointer))

;; some example handlers to light LEDs on corresponding
;; button presses

(defcallback dummy-handle-button-down :void ((monome-event :pointer) (data :pointer))
  (declare (ignore data))
  (with-foreign-pointer (*x 4)
    (with-foreign-pointer (*y 4)
      (with-foreign-pointer (**monome 4)
	(monome-event-get-grid monome-event *x *y **monome)
	(let ((x (mem-ref *x :uint))
	      (y (mem-ref *y :uint)))
	  (print (list 'press x y))
	  (monome-led-on (mem-ref **monome :pointer) x y))))))

(defcallback dummy-handle-button-up :void ((monome-event :pointer) (data :pointer))
  (declare (ignore data))
  (with-foreign-pointer (*x 4)
    (with-foreign-pointer (*y 4)
      (with-foreign-pointer (**monome 4)
	(monome-event-get-grid monome-event *x *y **monome)
	(let ((x (mem-ref *x :uint))
	      (y (mem-ref *y :uint)))
	  (print (list 'release x y))
	  (monome-led-off (mem-ref **monome :pointer) x y))))))

(defcfun "monome_event_loop" :pointer (monome :pointer))

#+nil
(defparameter *monome-dev* (monome-open "/dev/ttyUSB0" (cffi:null-pointer)))

#+nil
(monome-register-button-press *monome-dev* (callback dummy-handle-press))
#+nil
(monome-register-button-release *monome-dev* (callback dummy-handle-release))

#+nil
(monome-led-level-all *monome-dev* 17)
