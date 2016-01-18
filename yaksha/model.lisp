(in-package #:yaksha)

(defstruct model
  filepath
  meshes)

(defun load-model (filepath)
  (make-model
   :filepath filepath
   :meshes (load-assimp-meshes filepath)))

(defmethod print-object ((object model) stream)
  (format stream "#<lark-model ~s>" (sxhash object)))

(defmethod free ((object model))
  (mapcar #'free (model-meshes object))
  t)
