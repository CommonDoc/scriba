(defsystem scriba-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:scriba
               :fiveam)
  :description "Tests for Scriba."
  :components ((:module "t"
                :serial t
                :components
                ((:file "parser")
                 (:file "plump")
                 (:file "emitter")
                 (:file "scriba")))))
