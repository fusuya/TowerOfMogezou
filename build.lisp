(ql:quickload :ftw)

(defpackage caske2021au
  (:use :cl :ftw :cffi))


(loop :for file :in '("assetspath.lisp" "./src/define.lisp" 
                      "./src/stage.lisp" "./src/render.lisp"
                      "./src/init.lisp" "./src/test.lisp")
      :do (load file :external-format :utf-8))


(sb-ext:save-lisp-and-die "towerofmogezou.exe" :toplevel #'caske2021au::moge
					      ;;:application-type :gui
					      :executable t :save-runtime-options t)
