(in-package :cl-user)
(defpackage scriba-test.parser
  (:use :cl :fiveam)
  (:import-from :esrap
                :parse)
  (:import-from :scriba.parser
                :unquoted-string
                :quoted-string
                :general-string
                :tag-name
                :tag-attribute
                :tag-attributes
                :tag-body
                :tag
                :document)
  (:export :parser))
(in-package :scriba-test.parser)

(def-suite parser
  :description "Scriba parser tests.")
(in-suite parser)

(defmacro with-parsing-rule ((rule) (&body valid-cases) (&body invalid-cases))
  `(progn
     ,@(loop for case in valid-cases collecting
         `(is
           (equal (parse ',rule ,(first case))
                  ,(second case))))
     ,@(loop for case in invalid-cases collecting
         `(signals esrap:esrap-error
            (parse ',rule ,case)))))

(test unquoted-string
  (with-parsing-rule (unquoted-string)
    ;; Valid
    (("a" "a")
     ("abc" "abc")
     ("abc123" "abc123")
     ("123" "123"))
    ;; Invalid
    ("abc "
     "1 2 3")))

(test quoted-string
  ;; Valid
  (with-parsing-rule (quoted-string)
    (("\"a\"" "a")
     ("\"abc\"" "abc")
     ("\"abc123\"" "abc123")
     ("\"123\"" "123")
     ("\"a b c\"" "a b c")
     ("\"with \\\"inner\\\"\"" "with \\\"inner\\\""))
    ;; Invalid
    ("test"
     "\"test"
     "\"test"
     "\"a \"b c\"")))

(test (general-string :depends-on quoted-string)
  (with-parsing-rule (general-string)
    (("\"test test\"" "test test")
      ("abc" "abc"))
    ()))

(test (tag-name :depends-on unquoted-string)
  ;; Valid
  (with-parsing-rule (tag-name)
    (("@test" "test")
     ("@test-tag" "test-tag"))
    ("test"
     "@"
     "@test @test")))

(test (tag-attribute :depends-on quoted-string)
  (with-parsing-rule (tag-attribute)
    (("abc=123" '("abc" . "123"))
     ("abc=\"123\"" '("abc" . "123")))
    ("a=1 ")))

(test (tag-attributes-list :depends-on tag-attribute)
  (with-parsing-rule (tag-attributes)
    (("[]" (list))
     ("[a=1]" '(("a" . "1")))
     ("[a=1 b=2]" '(("a" . "1")
                    ("b" . "2")))
     ("[test=test test2=\"test\"]" '(("test" . "test")
                                     ("test2" . "test")))
     ("[a=\"1\" b=\"\\\"2\\\"\"]" '(("a" . "1")
                                    ("b" . "\\\"2\\\""))))
    ("[ ]"
     "[a]"
     "[a=]"
     "[a= ]"
     "[a=1 2]")))

(test tag-body
  (with-parsing-rule (tag-body)
    (("()" nil)
     ("(text)" '("text"))
     ("(text text)" '("text text"))
     ("(@test())" '((:name "test"
                     :attrs nil
                     :body nil)))
     ("(@test() test)" '((:name "test"
                          :attrs nil
                          :body nil)
                         " test"))
     ("(test @ test)" '("test " "@" " test"))
     ("(test @test())" '("test "
                         (:name "test"
                          :attrs nil
                          :body nil)))
     ("(test @test() test)" '("test "
                              (:name "test"
                               :attrs nil
                               :body nil)
                              " test")))
    ("(")))

(test (tags :depends-on tag-body)
  (with-parsing-rule (tag)
    ;; Valid
    (("@test()" '(:name "test"
                  :attrs nil
                  :body nil))
      ("@test[a=1]()" '(:name "test"
                        :attrs (("a" . "1"))
                        :body nil))
      ("@test[]()" '(:name "test"
                     :attrs nil
                     :body nil))
      ("@test[a=1]()" '(:name "test"
                        :attrs (("a" . "1"))
                        :body nil))
      ("@test[a=1 b=2]()" '(:name "test"
                            :attrs (("a" . "1")
                                    ("b" . "2"))
                            :body nil))
      ("@test(text)" '(:name "test"
                       :attrs nil
                       :body ("text"))))
    ;; Invalid
    ("@test")))

(test (empty-tags :depends-on tags)
  (with-parsing-rule (tag)
    ;; Valid
    (("()" '(:name "div"
             :attrs nil
             :body nil))
     ("(())" '(:name "div"
               :attrs nil
               :body ((:name "div"
                       :attrs nil
                       :body nil))))
     ("@node(())" '(:name "node"
                    :attrs nil
                    :body ((:name "div"
                            :attrs nil
                            :body nil)))))
    ()))

(test (nested-tags :depends-on tags)
  (with-parsing-rule (tag)
    (("@a(@b())" '(:name "a"
                   :attrs nil
                   :body ((:name "b"
                           :attrs nil
                           :body nil))))
      ("@a(@b() @c())" '(:name "a"
                         :attrs nil
                         :body ((:name "b"
                                 :attrs nil
                                 :body nil)
                                " "
                                (:name "c"
                                 :attrs nil
                                 :body nil)))))
    ()))

(test (document :depends-on nested-tags)
  (with-parsing-rule (document)
    (("test"
      '(:document "test"))
     ("test @test()"
      '(:document "test "
                  (:name "test"
                   :attrs nil
                   :body nil))))
    ()))

(test (parse-string :depends-on document)
  (is
    (equal (parse 'document "test")
           (list :document "test"))))
