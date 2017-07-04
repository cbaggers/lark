(in-package :lark)

(defvar auto-rot t)

(defun step-game ()
  ;; update mouse pos
  (swap-mouse-move)

  ;; switch rotation mode
  (when (key-down-p key.1)
    (setf auto-rot t))
  (when (key-down-p key.2)
    (setf auto-rot nil))

  (map nil #'step-thing (game-state-things *game-state*)))

(defun step-thing (thing)
  (let ((time (/ (now) 10200)))
    (if auto-rot
        (setf (thing-rot thing) (q:from-mat3
                                 (m3:rotation-from-euler
                                  (v! (* 2 (cos time))
                                      (sin time)
                                      (sin time)))))
        (when (mouse-down-p mouse.left)
          (let ((v (v2:/s (mouse-pos (mouse 0)) 100s0)))
            (setf (thing-rot thing) (q:from-mat3
                                     (m3:rotation-from-euler
                                      (v! (y v) 0s0 (x v))))))))))
