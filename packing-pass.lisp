(in-package :lark)

;;----------------------------------------------------------------------

(defun-g pack-gbuffer-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (let* ((pos (* (pos vert) 3.9))
         (m->w (m4:to-mat3 (get-transform model-space *world-space*)))

         (normal (normalize (* m->w (yaksha:normal vert))))
         (up-vec (v! 0 1 0))

         (tangent (normalize (cross normal up-vec)))
         (bitangent (normalize (cross normal tangent)))

         (btn-mat (m! tangent bitangent normal)))

    (values (in *clip-space*
              (in model-space (sv! pos 1.0)))
            (in *world-space*
              (in model-space (sv! pos 1.0)))
            btn-mat
            (yaksha:uv vert)
            normal)))


(defun-g pack-gbuffer-frag ((world-pos :vec4) (btn-mat :mat3) (uv :vec2) (misc :vec3)
                            &uniform (base-tex :sampler-2d)
                            (norm-tex :sampler-2d)
                            (mat-tex :sampler-2d)
                            (metallic-tex :sampler-2d)
                            (roughness-tex :sampler-2d))
  (let* ((met (pow (x (texture metallic-tex uv)) 2.2))    ;; [TODO] remove these pows and load in in srgb
         (rough (pow (x (texture roughness-tex uv)) 2.2))
         (ts-norm (- (* (s~ (texture norm-tex uv) :xyz) 2) (v3! 1)))
         (wnormal (normalize (* btn-mat ts-norm))))
    (values world-pos
            wnormal
            (s~ (texture base-tex uv) :xyz)
            ;;(nineveh::mipmap-level->grey base-tex uv)
            (v! met rough 0))))


(def-g-> pack-gbuffer-pass ()
  (pack-gbuffer-vert yaksha:vertex)
  (pack-gbuffer-frag :vec4 :mat3 :vec2 :vec3))

;;----------------------------------------------------------------------


(defun render-thing (thing camera render-state)
  (with-slots (gbuffer dfg)
      render-state
    (using-camera camera
      (loop :for mesh :in (yaksha:model-meshes (model thing)) :do
         (with-fbo-bound ((fbo gbuffer))
           (map-g #'pack-gbuffer-pass (yaksha:mesh-stream mesh)
                  :model-space (model-space thing)
                  :base-tex (base-sampler thing)
                  :norm-tex (normal-sampler thing)
                  :metallic-tex (metallic-sampler thing)
                  :roughness-tex (roughness-sampler thing)))))))
