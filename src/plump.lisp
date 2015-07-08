(in-package :cl-user)
(defpackage scriba.plump
  (:use :cl)
  (:export :parse)
  (:documentation "Parse the Scriba AST into Plump nodes."))
(in-package :scriba.plump)

(defvar *strip* nil)

(defun transform-blocks (ast)
  "Take pairs of @begin/@end blocks and turn the into single blocks."
  (if (stringp ast)
      ast
      (case (first ast)
        (:document
         (cons :document (transform-blocks (rest ast))))
        (:name
         (list :name (getf ast :name)
               :attrs (getf ast :attrs)
               :body (transform-blocks (getf ast :body))))
        (t
         (let ((stack (list (list)))
               (contexts (list)))
           (flet ((push-to-stack (elem)
                    (setf (first stack)
                          (append (first stack)
                                  (list elem)))))
             (loop for elem in ast do
               (cond
                 ((stringp elem)
                  (push-to-stack (if *strip*
                                     (prog1
                                         (string-left-trim '(#\Newline)
                                                           elem)
                                       (setf *strip* nil))
                                     elem)))
                 ((eq (first elem) :name)
                  (cond
                    ((equal (getf elem :name) "begin")
                     (setf *strip* t)
                     (push (list :name (first (getf elem :body))
                                 :attrs (getf elem :attrs))
                           contexts)
                     (push (list) stack))
                    ((equal (getf elem :name) "end")
                     (setf *strip* nil)
                     (let ((current (pop contexts))
                           (stack-frame (pop stack)))
                       (assert (equal (getf current :name)
                                      (first (getf elem :body))))
                       (push-to-stack
                        (list :name (getf current :name)
                              :attrs (getf current :attrs)
                              :body stack-frame))))
                    (t
                     (push-to-stack elem)))))))
           (first stack))))))

(defun ast-to-plump-sexp (ast)
  "Take an AST S-exp and turn it into a format plump-sexp likes."
  (if (stringp ast)
      ast
      (case (first ast)
        (:document
         (ast-to-plump-sexp (cons "scriba-root-node" (rest ast))))
        (:name
         (cond
           ((equal (getf ast :name) "div")
            (append (list (list "div"))
                    (list "(")
                    (ast-to-plump-sexp (getf ast :body))
                    (list ")")))
           (t
            (append
             (list (cons (getf ast :name)
                         (loop for (key . val) in (getf ast :attrs) appending
                           (list (intern key (find-package :keyword))
                                 val))))
             (ast-to-plump-sexp (getf ast :body))))))
        (t
         (loop for elem in ast collecting
           (ast-to-plump-sexp elem))))))

;;; Interface

(defun parse (string)
  "Parse a string of Scriba text into a Plump node."
  (let ((node (plump-sexp:parse
               (ast-to-plump-sexp
                (transform-blocks
                 (scriba.parser:parse-string string))))))
    (plump:make-root (plump:children (elt (plump:children node) 0)))))
