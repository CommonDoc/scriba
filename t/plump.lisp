(in-package :cl-user)
(defpackage scriba-test.plump
  (:use :cl :fiveam)
  (:import-from :scriba.plump
                :parse)
  (:export :plump))
(in-package :scriba-test.plump)

(def-suite plump
  :description "Scriba Plump tests.")
(in-suite plump)

(defmacro test-equal (scriba xml)
  `(is
    (equal (with-output-to-string (stream)
             (plump:serialize (parse ,scriba) stream))
           ,xml)))

(test text
  (test-equal "test"
              "test")
  (test-equal "test test test"
              "test test test"))

(test tag
  (test-equal "@test()"
              "<test/>")
  (test-equal "@test(a)"
              "<test>a</test>"))

(test nested-tags
  (test-equal "@a(@b())"
              "<a><b/></a>")
  (test-equal "@a(@b())"
              "<a><b/></a>")
  (test-equal "@a(@b(@c()))"
              "<a><b><c/></b></a>")
  (test-equal "@a(@b()@c())"
              "<a><b/><c/></a>"))

(test begin-end-blocks
  (test-equal "@begin(a)@begin(b)test@end(b)@begin(c)text@end(c)@end(a)"
              "<a><b>test</b><c>text</c></a>"))

(test attributes
  (test-equal "@node[attr=val]()"
              "<node attr=\"val\"/>"))
