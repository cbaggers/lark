(in-package :lark)

(let ((mouse-movement (v! 0 0))
      (last-frame-relative-movement (v! 0 0)))
  (defun receive-mouse-movement (source timestamp)
    (declare (ignore timestamp))
    (setf last-frame-relative-movement
	  (v2:+ last-frame-relative-movement
		(skitter:xy-pos-relative source))))
  (defun mouse-movement ()
    mouse-movement)
  (defun swap-mouse-move ()
    (setf mouse-movement last-frame-relative-movement
	  last-frame-relative-movement (v! 0 0))))

(defun enable-mouse-capture ()
  (sdl2:set-relative-mouse-mode :true))

(defun disable-mouse-capture ()
  (sdl2:set-relative-mouse-mode :true))

(defparameter *mouse-move-listener*
  (progn
    (when (and (boundp '*mouse-move-listener*)
	       (symbol-value '*mouse-move-listener*))
      (skitter:stop-listening *mouse-move-listener*))
    (skitter:listen-to (skitter:make-event-listener #'receive-mouse-movement)
		       (skitter:mouse 0)
		       :pos)))
