(in-package :cl-monome)

(defvar *monome-output-stream*)

(defun setup-monome-dev (&optional (dev-file "/dev/ttyUSB0"))
  (external-program:run "stty" (list "-F" dev-file "115200" "sane" "-brkint" "-icrnl" "-opost" "-onlcr" "-isig" "-icanon" "-iexten" "-echo" "-echoe")))

(defmacro with-monome-output-stream ((&optional (monome-output-stream '*monome-output-stream*)
						(monome-dev-file "/dev/ttyUSB0"))
				     &body body)
  `(with-open-file (,monome-output-stream ,monome-dev-file
					  :direction :output
					  :if-exists :overwrite
					  :element-type '(unsigned-byte 8))
     ,@body))

(defvar *monome-input-stream*)

(defmacro with-monome-input-stream ((&optional (monome-input-stream '*monome-input-stream*)
						(monome-dev-file "/dev/ttyUSB0"))
				     &body body)
  `(progn
     (setup-monome-dev ,monome-dev-file)
     (with-open-file (,monome-input-stream ,monome-dev-file
					   :direction :input
					   :if-exists :overwrite
					   :element-type '(unsigned-byte 8))
       ,@body)))

(defclass monome-button-event ()
  ((x :initarg :x) (y :initarg :y)))
(defclass monome-button-press (monome-button-event)
  ())
(defclass monome-button-release (monome-button-event)
  ())

(defun monome-receive-message (&optional (monome-input-stream *monome-input-stream*))
  (let ((byte (read-byte monome-input-stream)))
    (match byte
      (#x21 (return-from monome-receive-message
	      (make-instance 'monome-button-press
			     :x (read-byte monome-input-stream)
			     :y (read-byte monome-input-stream))))
      (#x20 (return-from monome-receive-message
	      (make-instance 'monome-button-release
			     :x (read-byte monome-input-stream)
			     :y (read-byte monome-input-stream)))))))

(defmacro def-monome-cmd (spec args &body body)
  `(defun ,spec ,(if (find '&optional args)
		     (append args '((monome-output-stream *monome-output-stream*)))
		     (append args '(&optional (monome-output-stream *monome-output-stream*))))
     ,@body
     (force-output monome-output-stream)))

(defmacro monome-send-bytes (&rest bytes)
  `(progn ,@(mapcar (lambda (byte)
		      `(write-byte ,byte monome-output-stream))
		    bytes)))

(def-monome-cmd monome-request-device-info ()
  (monome-send-bytes #x00))

(def-monome-cmd monome-request-id-string ()
  (monome-send-bytes #x01))

(def-monome-cmd monome-request-grid-size ()
  (monome-send-bytes #x05))

(def-monome-cmd monome-set-led (x y)
  (monome-send-bytes #x11 x y))

(def-monome-cmd monome-unset-led (x y)
  (monome-send-bytes #x10 x y))

(def-monome-cmd monome-set-all ()
  (monome-send-bytes #x13))

(def-monome-cmd monome-unset-all ()
  (monome-send-bytes #x12))

(defun coerce-to-binary (thing)
  (typecase thing
    (number (cond ((<= thing 0) 0)
		  (t 1)))
    (null 0)
    (t 1)))

(defun pack-byte (row)
  (loop for el in row
     for i below (length row)
     sum (ash (coerce-to-binary el) i)))

(def-monome-cmd monome-map-leds (x y 8x8-subgrid)
  (assert (= (length 8x8-subgrid) 8))
  (assert (every (lambda (row)
  		   (= (length row)
  		      8))
  		 8x8-subgrid))
  (assert (= 0 (rem x 8)))
  (assert (= 0 (rem y 8)))
  (monome-send-bytes #x14 x y)
  (loop for row in 8x8-subgrid
     do (monome-send-bytes (pack-byte row))))
		 
(def-monome-cmd monome-set-row (x y 8x1-row)
  (monome-send-bytes #x15 x y (pack-byte 8x1-row)))

(def-monome-cmd monome-set-column (x y 8x1-col)
  (monome-send-bytes #x16 x y (pack-byte 8x1-col)))

(def-monome-cmd monome-set-intensity (i)
  (monome-send-bytes #x17 i))

(def-monome-cmd monome-set-led-intensity (x y i)
  (monome-send-bytes #x18 x y i))

(def-monome-cmd monome-set-all-intensity (i)
  (monome-send-bytes #x19 i))

(defun pack-nibbles (hi-nib lo-nib)
  (+ (ash hi-nib 4)
     lo-nib))

(def-monome-cmd monome-map-intensities (x y 8x8-subgrid)
  (assert (= (length 8x8-subgrid) 8))
  (assert (every (lambda (row)
  		   (= (length row)
  		      8))
  		 8x8-subgrid))
  (monome-send-bytes #x1a x y)
  (loop for row in 8x8-subgrid
     do (loop for (hb lb) on row by #'cddr
	   do (monome-send-bytes (pack-nibbles hb lb))))
  (sleep 0.000001))

(def-monome-cmd monome-row-intensities (x y 8x1-row)
  (monome-send-bytes #x1b x y)
  (loop for (hb lb) on 8x1-row by #'cddr
     do (monome-send-bytes (pack-nibbles hb lb))))

(def-monome-cmd monome-col-intensities (x y 8x1-col)
  (monome-send-bytes #x1c x y)
  (loop for (hb lb) on 8x1-col by #'cddr
     do (monome-send-bytes (pack-nibbles hb lb))))
