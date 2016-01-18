(in-package #:yaksha)
(in-readtable fn:fn-reader)

(jungl:defstruct-g vertex ()
  (position :vec3 :accessor pos)
  (normal :vec3 :accessor normal)
  (tex-coords :vec2 :accessor uv))

(defstruct texture
  jungl-texture
  type)

(defstruct mesh
  stream ;; will retain the arrays in here
  (textures nil :type list))

(defgeneric free (object))

(defmethod free ((object t))
  nil)

(defmethod free ((object mesh))
  (jungl:free-vertex-stream (mesh-stream object))
  (mapcar #'free (mesh-textures object))
  t)

(defmethod free ((object texture))
  (jungl:free-texture (texture-jungl-texture object))
  t)

(defmethod print-object ((object mesh) stream)
  (format stream "#<lark-mesh :verts ~s>"
	  (jungl::vertex-stream-length (mesh-stream object))))

(defmethod print-object ((object texture) stream)
  (format stream "#<lark-texture :dimensions ~s>"
	  (jungl:dimensions
	   (jungl:texref
	    (texture-jungl-texture object)))))

;;----------------------------------------------------------------------

(defun load-assimp-meshes (filename)
  (let* ((scene (file->scene filename))
	 (meshes (assimp-scene->meshes scene filename)))
    meshes))

(defun file->scene (filename)
  (assimp:import-into-lisp filename :processing-flags '(:ai-process-triangulate
							:ai-process-flip-u-vs)))

(defun assimp-scene->meshes (a-scene model-filename)
  (let ((materials (assimp:materials a-scene))
	(texture-cache (make-hash-table :test #'equal)))
    (map 'list λ(assimp-mesh->mesh _ materials model-filename texture-cache)
	 (assimp:meshes suit))))

(defun assimp-mesh->mesh (a-mesh materials model-filename texture-cache)
  (let* ((vertex-gpu-array (a-mesh->vertex-gpu-array a-mesh))
	 (index-gpu-array (a-mesh->indicies-gpu-array a-mesh))
	 (textures (a-mesh->textures a-mesh materials model-filename
				     texture-cache)))
    (make-mesh
     :stream (jungl:make-buffer-stream vertex-gpu-array
				       :index-array index-gpu-array
				       :retain-arrays t)
     :textures textures)))

(defun a-mesh->indicies-gpu-array (a-mesh)
  (jungl:make-gpu-array
   (reduce λ(concatenate 'list _ _1) (assimp:faces a-mesh))
   :element-type :ushort))

(defun a-mesh->textures (a-mesh materials model-filename texture-cache)
  (let ((index (assimp:material-index a-mesh)))
    (when (> index 0)
      (a-material->textures (aref materials index) texture-cache model-filename))))

(defun a-material->textures (material texture-cache model-filename)
  (let* ((tex-files (gethash "$tex.file" material))
	 (tex-type-filter '(:ai-texture-type-specular
			    :ai-texture-type-diffuse)))
    (remove nil (mapcar λ(when (member (first _) tex-type-filter)
			   (tex-file->texture _ texture-cache model-filename))
			tex-files))))

(defun tex-file->texture (tex-file texture-cache model-filename)
  (dbind (a-tex-type ? tex-filename) tex-file
    (declare (ignore ?))
    (let ((filepath (cl-fad:merge-pathnames-as-file model-filename
						    tex-filename)))
      (or (gethash tex-filename texture-cache)
	  (let ((tex (make-texture
		      :jungl-texture (devil-helper:load-image-to-texture filepath)
		      :type (a-tex-type-name->lark-tex-type-name a-tex-type))))
	    (setf (gethash tex-filename texture-cache) tex)
	    (cepl::peek texture-cache)
	    tex)))))

(defun a-tex-type-name->lark-tex-type-name (name)
  (let ((mapping '((:ai-texture-type-none . :none)
		   (:ai-texture-type-diffuse . :diffuse)
		   (:ai-texture-type-specular . :specular)
		   (:ai-texture-type-ambient . :ambient)
		   (:ai-texture-type-emissive . :emissive)
		   (:ai-texture-type-height . :height)
		   (:ai-texture-type-normals . :normals)
		   (:ai-texture-type-shininess . :shininess)
		   (:ai-texture-type-opacity . :opacity)
		   (:ai-texture-type-displacement . :displacement)
		   (:ai-texture-type-lightmap . :lightmap)
		   (:ai-texture-type-reflection . :reflection)
		   (:ai-texture-type-unknown . :unknown))))
    (or (cdr (assoc name mapping))
	(error "Assimp texture type ~s not found in lark's mapping" name))))

(defun a-mesh->vertex-gpu-array (a-mesh)
  (let ((positions (assimp:vertices a-mesh))
	(normals (assimp:normals a-mesh))
	(uvs (a-mesh->uvs a-mesh)))
    (assert (= (length positions)
	       (length normals)
	       (length uvs)))
    (jungl:make-gpu-array
     (map 'list #'list positions normals uvs)
     :element-type 'vertex)))

(defun a-mesh->uvs (a-mesh)
  (let ((len (elt (assimp:components-per-texture-coord a-mesh) 0)))
    (when (not (= 2 len))
      (error "can only support 2 component uvs right now"))
    ;; elt 0 as only pull set of uvs right now ----------------^-----v
    (map 'list λ(v:s~ _ :xy) (elt (assimp:texture-coords a-mesh) 0))))
