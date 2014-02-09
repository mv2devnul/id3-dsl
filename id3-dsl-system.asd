(asdf:defsystem #:id3-dsl-system
  :depends-on (#:com.gigamonkeys.binary-data #:chanl #:flexi-streams #:cl-fad #:zlib)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "id3-dsl"  :depends-on ("packages" "utils"))))
