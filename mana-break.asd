(defsystem "mana-break"
  :version "0.0.2"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-tiled
               #:global-vars
               #:let-plus
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "components")
                 (:file "ui")
                 (:file "atlas")
                 (:file "sprite")
                 (:file "map")
                 (:file "behaviour-tree")
                 (:file "priority-queue")
                 (:file "game")
                 (:file "character")
                 (:file "main"))))
  :description "A simple game."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"mana-break"
  :entry-point "mana-break:main")
