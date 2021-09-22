(ql:quickload :ftw)

(defpackage caske2021au
  (:use :cl :ftw :cffi))


(loop :for file :in '("define.lisp" "stage.lisp" "init.lisp"  "render.lisp"
                      "test.lisp")
      :do (load file :external-format :utf-8))
