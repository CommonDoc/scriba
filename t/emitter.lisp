(in-package :cl-user)
(defpackage scriba-test.emitter
  (:use :cl :fiveam)
  (:export :emitter))
(in-package :scriba-test.emitter)

(def-suite emitter
  :description "Scriba emitter tests.")
(in-suite emitter)

(defun parse (string)
  (common-doc-plump.parser:parse
   (scriba.plump:parse string)))

(defmacro emit-equal (node output)
  `(is
    (equal (with-output-to-string (stream)
             (scriba.emitter:emit ,node stream))
           ,output)))

(defmacro parse-equal (input output)
  `(emit-equal (parse ,input) ,output))

(defmacro emit-identity (input)
  `(parse-equal ,input ,input))

(defmacro markup-tests ()
  `(test markup
     ,@(loop for tag-name in (list "b" "i" "u" "strike" "c" "sup" "sub")
         collecting
         `(emit-identity ,(concatenate 'string "@" tag-name "(text)")))))

(markup-tests)

(test paragraph
  (emit-identity "para1

para2

para3

"))

(test code-block
  (emit-identity "@begin[lang=lisp](code)
test
@end(code)"))

(test quotes
  (emit-identity "@q(test)")
  (emit-identity "@begin(quote)
test
@end(quote)"))

(test lists
  (emit-identity "@begin(list)
@item(a)
@end(list)")
  (emit-identity "@begin(list)
@item(a)
@item(b)
@end(list)")
  (emit-identity "@begin(enum)
@item(a)
@item(b)
@end(enum)")
  (emit-identity "@begin(deflist)
@term(a)
@def(1)
@end(deflist)"))

(test images
  (emit-identity "@image[src=a]()"))

(test tables
  (emit-identity "@begin(table)
@begin(row)
@cell(a)
@end(row)
@end(table)"))

(test section
  (emit-identity "@begin(section)
@title(sec)

test

@end(section)"))

(test web-link-1
  (emit-identity "Visit @link[uri=https://www.google.com/](Google)."))

(test web-link-2
  (parse-equal "Visit @link[uri=\"https://www.google.com/\"](Google)."
               ;; Note, emitter outputs uri without double quotes:
               "Visit @link[uri=https://www.google.com/](Google)."))

(test ref-link-1
  (emit-identity "See the third @ref[id=data-table-3](table) for the data."))

(test ref-link-2
  (parse-equal "See the third @ref[id=\"data-table-3\"](table) for the data."
               ;; Note, emitter outputs uri without double quotes:
               "See the third @ref[id=data-table-3](table) for the data."))

(test ref-link-3
  (emit-identity "See the third @ref[doc=foo-bar,id=data-table-3](table) for the data."))

(test ref-link-4
  (parse-equal "See the third @ref[doc=foo-bar id=\"data-table-3\"](table) for the data."
               ;; Note, emitter removes a space between attributes
               "See the third @ref[doc=foo-bar id=data-table-3](table) for the data."))

(test ref-link-5
  (emit-identity "See the third @ref[id=\"foo-bar doc=data-table-3\"](table) for the data."))
