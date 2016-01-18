;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils)
  (:export :load-model :vertex :mesh :make-mesh :model :make-model))

(defpackage #:lark
  (:use #:cl))
