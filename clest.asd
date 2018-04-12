;;;; clest.asd

(asdf:defsystem #:clest
  :description "CL tests go east."
  :author "Daniel Kochmański"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:1am #:alexandria #:mcclim)
  :serial t
  :components ((:file "package")
               (:file "clest")          ; utilities and mixins
               (:module "core"
                        :description "Core protocol and basic implementation."
                        :components ((:file "protocol")
                                     (:file "world") ; default root node
                                     (:file "basic-project")
                                     (:file "basic-test-suite")
                                     (:file "basic-test-scenario")
                                     #+ (or) (:file "test-case")))
               (:module "tests"
                        :description "Unit tests of the protocol."
                        :components ((:file "unit-tests")))
               (:module "interface"
                        :description "CLIM interface."
                        :components ((:file "application-frame")
                                     (:file "tree-pane")
                                     (:file "tree-gadget")))))
