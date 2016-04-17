(in-package #:lark)

(grab-bag:def-typed-bag gpu-array-pool %cepl.types:gpu-array-bb
  %cepl.types:+null-buffer-backed-gpu-array+)

(grab-bag:def-typed-bag gpu-stream-pool cepl:buffer-stream
  (%cepl.types:make-uninitialized-buffer-stream))

(defvar *gpu-array-pool* (bag-of-gpu-array-bb!))
(defvar *gpu-stream-pool* (bag-of-buffer-stream!))

(defgeneric add-to-release-pool (object))

(defmethod add-to-release-pool ((object %cepl.types:gpu-array-bb))
  (add-item-to-gpu-array-bb-bag *gpu-array-pool* object)
  object)

(defmethod add-to-release-pool ((object cepl:buffer-stream))
  (add-item-to-buffer-stream-bag *gpu-stream-pool* object)
  object)
