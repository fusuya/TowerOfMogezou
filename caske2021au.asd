(defsystem "caske2021au"
  :version "1.0.0"
  :author "mogezou"
  :license ""
  :depends-on (:ftw)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "define");;    :depends-on ("package"))
		 (:file "maze-test");;     :depends-on ("package"))
		 ;;(:file "mci");;       :depends-on ("define"))
		 (:file "item")
		 (:file "astar")
		 (:file "render")
		 (:file "meimoge");; :depends-on ("define"))
		
		 
		 )))
  :description "")
