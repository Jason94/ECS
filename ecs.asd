(defsystem "ecs"
  :long-name "Coalton Entity-Component-System"
  :version "0.1"
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license "MIT"
  :depends-on ("coalton" "named-readtables" "coalton-io" "cl-raylib" "3d-vectors")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "vectors")
                 (:file "core")
                 (:file "common")
                 ;; (:file "ltk")
                 (:file "raylib")
                 )))
  )
