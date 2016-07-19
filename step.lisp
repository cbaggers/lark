(in-package :lark)

(defvar auto-rot t)

(defun step-game ()
  ;; update mouse pos
  (swap-mouse-move)

  ;; switch rotation mode
  (when (skitter:key-down-p key.1)
    (setf auto-rot t))
  (when (skitter:key-down-p key.2)
    (setf auto-rot nil))

  ;; hacky rotation
  (let ((time (/ (now) 10200))
	(thing (first (things *game-state*))))
    (if auto-rot
	(setf (rot thing) (q:from-mat3
			   (m3:rotation-from-euler
			    (v! (* 2 (cos time))
				(sin time)
				(sin time)))))
	(let ((v (v2:/s (mouse-pos) 100s0)))
	  (setf (rot thing) (q:from-mat3
			     (m3:rotation-from-euler
			      (v! (y v) 0s0 (x v)))))))))
