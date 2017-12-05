(in-package :monome-serialosc)
(defclass monome-button-event ()
  ((x :initarg :x) (y :initarg :y)))
(defclass monome-button-press (monome-button-event)
  ())
(defclass monome-button-release (monome-button-event)
  ())

(defvar *monome-devices* nil)

(defvar *app-osc-port* 6666)

(defun detect-monome-devices ()
  "call this function to index monome devices from serial osc master control program"
  (let ((in (socket-connect nil nil
			    :local-port *app-osc-port*
			    :local-host #(127 0 0 1)
			    :protocol :datagram
			    :element-type '(unsigned-byte 8)
			    :timeout 0.1
			    ))
	(out (socket-connect #(127 0 0 1) 12002
			     :protocol :datagram
			     :element-type '(unsigned-byte 8)))
	(buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (unwind-protect
	 (let ((mess (osc:encode-message "/serialosc/list" "localhost" *app-osc-port*)))
	   (socket-send out mess (length mess))
	   (let ((monomes nil))
	     (loop while (wait-for-input in
					 :timeout 0.1
					 :ready-only t)
		do (socket-receive in buffer (length buffer))
		  (push (osc:decode-bundle buffer)
			monomes))
	     (setf *monome-devices* monomes)))
      (when in (socket-close in))
      (when out (socket-close out)))))

(defun setup-monome-dev ()
  (detect-monome-devices))

(defvar *default-monome-led-port* nil)

(defun open-monome-led-port (&optional (idx 0))
  (when *default-monome-led-port*
    (socket-close *default-monome-led-port*))
  (setf *default-monome-led-port*
	(socket-connect #(127 0 0 1) (nth 3 (nth idx *monome-devices*))
		  :protocol :datagram
		  :element-type '(unsigned-byte 8))))

(defmacro with-monome-output ((&optional (monome-output-sock '*default-monome-led-port*)
					 (idx 0))
				     &body body)
  `(let ((,monome-output-sock (socket-connect #(127 0 0 1) (nth 3 (nth ,idx *monome-devices*))
					      :protocol :datagram
					      :element-type '(unsigned-byte 8))))
     (unwind-protect
	  (progn ,@body)
       (when ,monome-output-sock
	 (socket-close ,monome-output-sock)))))

(defvar *default-monome-key-port* nil)
(defun open-monome-key-port (&optional (idx 0))
  (when *default-monome-led-port*
    (socket-close *default-monome-key-port*))
  (setf *default-monome-key-port*
	(socket-connect nil nil
			:local-port (+ *app-osc-port* 1 idx)
			:local-host #(127 0 0 1)
			:protocol :datagram
			:element-type '(unsigned-byte 8)
			:timeout 1
			)))

(defmacro with-monome-input ((&optional (monome-input-sock '*default-monome-key-port*)
					(idx 0))
			     &body body)
  `(let ((,monome-input-sock (socket-connect nil nil
			:local-port (+ *app-osc-port* 1 ,idx)
			:local-host #(127 0 0 1)
			:protocol :datagram
			:element-type '(unsigned-byte 8)
			:timeout 1
			)))
     (unwind-protect
	  (progn ,@body)
       (when ,monome-input-sock
	 (socket-close ,monome-input-sock)))))

(defvar *focus* nil)

(defun grab-focus (&optional (idx 0))
  (let ((buf (osc:encode-message
	      "/sys/port"
	      (+ *app-osc-port* 1 idx))))
    (handler-case
	(socket-send *default-monome-led-port*
		     buf (length buf))
      (usocket:connection-refused-error (e)
	(declare (ignore e))))))

(defun monome-wait-key ()
  (let ((buf (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (when (wait-for-input *default-monome-key-port*
			  :ready-only t)
      (osc:decode-message (socket-receive *default-monome-key-port*
					  buf (length buf))))))
(defun monome-receive-message ()
  (let ((buf (make-sequence '(vector (unsigned-byte 8)) 1024)))
      (match (osc:decode-message (socket-receive *default-monome-key-port*
						 buf (length buf)))
	((list "/monome/grid/key" x y 1)
	 (make-instance 'monome-button-press :x x :y y))
	((list "/monome/grid/key" x y 0)
	 (make-instance 'monome-button-release :x x :y y))
	((list "/sys/port" p)
	 (list :focus (setf *focus* (= p (+ *app-osc-port* 1)))))
	(default default);; pass unmatched osc messages un-interpreted
	)))

(defun monome-send-message (address &rest args)
  (when *focus*
    (let ((mess (apply #'osc:encode-message
		       (cons address args))))
      (handler-case
	  (socket-send *default-monome-led-port*
		       mess (length mess))
	(usocket:connection-refused-error (e)
	  (declare (ignore e)))))))

(defun monome-set-led-intensity (x y l)
  (monome-send-message "/monome/grid/led/level/set" x y l))

(defun monome-map-intensities (x y 8x8-subgrid)
  (assert (= (length 8x8-subgrid) 8))
  (assert (every (lambda (row)
  		   (= (length row)
  		      8))
  		 8x8-subgrid))
  (apply #'monome-send-message
	 (append (list "/monome/grid/led/level/map" x y)
			(loop for row in 8x8-subgrid
			   append (loop for nib in row
				     collect nib)
			     ))))
(defun monome-row-intensities (x y 8x1-row)
  (apply #'monome-send-message
	 (append (list "/monome/grid/led/level/row" x y)
		 (loop for i in 8x1-row
		    collect i))))

#+nil
(monome-map-intensities
 8 0
 (loop for i below 8
    collect (loop for j below 8
	       collect (+ i j))))

(defun monome-map-128 (16x8-grid)
    (assert (= (length 16x8-grid) 8))
    (assert (every (lambda (row)
		     (= (length row)
			16))
		   16x8-grid))
    (monome-map-intensities 0 0
			    (mapcar (lambda (row)
				      (subseq row 0 8))
				    16x8-grid))

    (monome-map-intensities 8 0
			    (mapcar (lambda (row)
				      (subseq row 8))
				    16x8-grid)))
#+nil
(monome-map-128
 (loop for i below 8
    collect (loop for j below 16
	       collect (+ i (round (/ j 2))))))



