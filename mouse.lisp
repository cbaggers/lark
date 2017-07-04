(in-package :lark)

(let ((mouse-movement (v! 0 0))
      (last-frame-relative-movement (v! 0 0)))
  (defun receive-mouse-movement (moved &rest ignored)
    (declare (ignore ignored))
    (setf last-frame-relative-movement
          (v2:+ last-frame-relative-movement moved)))
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
      (stop-listening *mouse-move-listener*))
    (listen-to #'receive-mouse-movement (mouse 0) :move)))
