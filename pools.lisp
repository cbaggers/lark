(in-package #:lark)

(grab-bag:def-typed-bag gpu-array-pool jungl::gpuarray
  (jungl::make-gpuarray))

(grab-bag:def-typed-bag gpu-stream-pool jungl::vertex-stream
  (jungl::make-raw-vertex-stream))

(defvar *gpu-array-pool* (bag-of-gpuarray!))
(defvar *gpu-stream-pool* (bag-of-vertex-stream!))

(defgeneric add-to-release-pool (object))

(defmethod add-to-release-pool ((object jungl::gpuarray))
  (add-item-to-gpuarray-bag *gpu-array-pool* object)
  object)

(defmethod add-to-release-pool ((object jungl::vertex-stream))
  (add-item-to-vertex-stream-bag *gpu-stream-pool* object)
  object)
