(in-package #:assurance)

(defvar *world-up* (v! 0 1 0))

(defvar *camera* nil)


(defmethod pos ((c cepl.camera:camera))
  (camera-pos c))

(defmethod rot ((c cepl.camera:camera))
  (camera-rot c))
