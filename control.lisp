;;----------------------------------------------------------------------

(defun-g pack-gbuffer-control-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (let* ((m->w (m4:to-mat3 (get-transform model-space *world-space*)))
         (normal (* m->w (- (* (yaksha:normal vert) 2) (v! 1 1 1))))
         (tangent (* m->w (yaksha:tangent vert)))
         (bitangent (cross normal tangent)))
    (values (in *clip-space*
              (in model-space (sv! (pos vert) 1.0)))
            (in *world-space*
              (in model-space (sv! (pos vert) 1.0)))
            (in *world-space*
              (sv! wnormal))
            (yaksha:uv vert))))

(defun-g pack-gbuffer-control-frag ((world-pos :vec4) (wnormal :mat3) (uv :vec2)
                                    &uniform (base-color :vec3) (metallic :float)
                                    (roughness :float))
  (values world-pos
          wnormal
          base-color
          (v! metallic roughness 1)))

(defpipeline-g pack-gbuffer-control-pass ()
  (pack-gbuffer-control-vert yaksha:vertex)
  (pack-gbuffer-control-frag :vec4 :mat3 :vec2))
