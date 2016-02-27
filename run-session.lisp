(ql:quickload :lark)

(funcall
 (symbol-function
  (find-symbol "%RUN-SESSION" (find-package "LARK"))))
