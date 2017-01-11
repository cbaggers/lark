(defun-g blinn-diffuse ((normal :vec3) (light-dir :vec3) (base-color :vec3))
  (max 0s0 (dot normal light-dir)))
