(in-package #:yaksha)

(defstruct model
  filepath
  meshes)

(defun load-model (filepath)
  (make-model
   :filepath filepath
   :meshes (load-assimp-meshes filepath)))

(defun make-model-from-mesh (mesh)
  (make-model :filepath "n/a"
              :meshes (list mesh)))

(defmethod print-object ((object model) stream)
  (format stream "#<lark-model ~s>" (sxhash object)))

(defmethod free ((object model))
  (mapcar #'free (model-meshes object))
  t)
