(in-package :cl-user)
(defpackage scriba.parser
  (:use :cl :esrap)
  (:export :unquoted-string
           :quoted-string
           :general-string
           :tag-name
           :tag-attribute
           :tag-attributes
           :tag-body
           :tag
           :text-node
           :general-node
           :document
           :parse-string)
  (:documentation "Parse Scriba text into S-expressions."))
(in-package :scriba.parser)

;;; Utilities

(defrule whitespace (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defun not-doublequote (char)
  (not (eql #\" char)))

(defrule escape-string (and #\\ #\")
  (:constant "\\\""))

;;; Strings

(defrule attribute-name-char (not (or whitespace #\= #\])))

;; a, abc, ab123
(defrule unquoted-string (+ attribute-name-char)
  (:lambda (text)
    (text text)))

(defrule string-char (or escape-string (not-doublequote character)))

;; "Hello, \"world\"!"
(defrule quoted-string (and #\" (* string-char) #\")
  (:destructure (open-quote text close-quote)
    (declare (ignore open-quote close-quote))
    (text text)))

;; a, ab, "123", "Hello, \"world\"!"
(defrule general-string (or quoted-string unquoted-string))

;;; Tags

(defrule tag-name (and #\@ (+ (not (or whitespace #\[ #\())))
  (:destructure (at-sign text)
    (declare (ignore at-sign))
    (text text)))

(defrule tag-attribute (and unquoted-string
                            (? whitespace) #\= (? whitespace)
                            general-string)
  (:destructure (key ws1 eq ws2 value)
    (declare (ignore ws1 eq ws2))
    (cons key value)))

(defrule tag-attributes (and #\[
                             (? tag-attribute)
                             (* (and whitespace tag-attribute))
                             #\])
  (:destructure (open-bracket first-attr attributes close-bracket)
    (declare (ignore open-bracket close-bracket))
    (if attributes
        (cons first-attr (loop for attr in attributes
                           collecting (second attr)))
        (if first-attr
            (list first-attr)))))

(defrule tag-body (and #\(
                       (* general-node)
                       #\))
  (:destructure (open-paren body close-paren)
    (declare (ignore open-paren close-paren))
    body))

(defrule tag (and (? (and tag-name (? tag-attributes))) tag-body)
  (:destructure (header body)
    (if header
        (destructuring-bind (name attributes) header
          (list :name name
                :attrs attributes
                :body body))
        (list :name "div"
              :attrs nil
              :body body))))

(defrule text-node (+ (not (or #\@ #\( #\))))
  (:lambda (text)
    (text text)))

(defrule at-sign-node (and #\@)
  (:constant "@"))

(defrule general-node (or tag text-node at-sign-node))

(defrule document (+ general-node)
  (:lambda (nodes)
    (cons :document nodes)))

(defun parse-string (string)
  "Parse a string of Scriba text into an S-expression."
  (parse 'document string))
