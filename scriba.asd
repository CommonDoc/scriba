(defsystem scriba
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:common-doc-plump
               :plump-sexp
               :esrap)
  :components ((:module "src"
                :serial t
                :components
                ((:file "parser")
                 (:file "plump")
                 (:file "emitter")
                 (:file "scriba"))))
  :description "A markup format similar to Scribe."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op scriba-test))))
