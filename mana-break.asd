(defsystem "mana-break"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:let-plus
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "components")
                 (:file "atlas")
                 (:file "sprite")
                 (:file "main"))))
  :description "A simple game."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"mana-break"
  :entry-point "mana-break:main")
