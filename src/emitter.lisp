(in-package :cl-user)
(defpackage scriba.emitter
  (:use :cl :common-doc)
  (:export :emit)
  (:documentation "Dump CommonDoc nodes to Scriba text."))
(in-package :scriba.emitter)

;;; Utilities

(defmacro with-tag-name ((name stream) &body body)
  `(progn
     (write-char #\@ ,stream)
     (write-string ,name ,stream)
     ,@body))

(defun emit-hash-table (hash-table stream)
  (flet ((string-needs-quoting-p (string)
           (if (position #\" string) t)))
    (when (> (hash-table-count hash-table) 0)
      (write-char #\[ stream)
      (loop for attr-name being the hash-keys of hash-table
            for attr-value being the hash-values of hash-table
            do
               (write-string attr-name stream)
               (write-char #\= stream)
               (if (string-needs-quoting-p attr-value)
                   (print attr-value stream)
                   (princ attr-value stream)))
      (write-char #\] stream))))

(defmacro with-tag-attrs ((attrs stream &key extra) &body body)
  (let ((hash-name (gensym)))
    `(let ((,hash-name (if ,attrs
                           (alexandria:copy-hash-table ,attrs)
                           (make-hash-table :test #'equal))))
       (when ,extra
         (loop for (key . value) in ,extra do
           (setf (gethash key ,hash-name) value)))
       (emit-hash-table ,hash-name ,stream)
       ,@body)))

(defmacro with-tag-body ((stream) &body body)
  `(progn
     (write-char #\( ,stream)
     ,@body
     (write-char #\) ,stream)))

(defmacro with-tag ((node stream &key name extra-attrs) &body body)
  (let ((name (if name
                  name
                  `(find-tag (class-of ,node)))))
    `(with-tag-name (,name ,stream)
       (with-tag-attrs ((if ,node (metadata ,node) nil) ,stream :extra ,extra-attrs)
         (with-tag-body (,stream)
           ,@body)))))

(defmacro with-block-tag ((node stream &key name extra-attrs) &body body)
  (let ((name (if name
                  name
                  `(find-tag (class-of ,node)))))
    `(progn
       (with-tag-name ("begin" ,stream)
         (with-tag-attrs ((if ,node (metadata ,node) nil) ,stream :extra ,extra-attrs)
           (with-tag-body (,stream)
             (write-string ,name ,stream))))
       ,@body
       (with-tag-name ("end" ,stream)
         (with-tag-body (,stream)
           (write-string ,name ,stream))))))

(defun trivial-tag (node stream)
  (with-tag (node stream)
    (emit-children node stream)))

;;; Generics and methods

(defgeneric emit (node stream)
  (:documentation "Emit a node."))

(defun emit-list (list stream)
  (loop for elem in list do
    (emit elem stream)))

(defun emit-children (node stream)
  "Emit the children of a node."
  (emit-list (children node) stream))

;;; Basic nodes

(defmethod emit ((node content-node) stream)
  (emit-children node stream))

(defmethod emit ((node text-node) stream)
  (write-string (text node) stream))

(defmethod emit ((node paragraph) stream)
  "Emit a paragraph."
  (emit-children node stream)
  (write-string (make-string 2 :initial-element #\Newline) stream))

;;; Markup

(defmethod emit ((node markup) stream)
  (trivial-tag node stream))

(defmethod emit ((node code-block) stream)
  (with-block-tag (node stream :extra-attrs (list (cons "lang" (language node))))
    (emit-children node stream)))

(defmethod emit ((node inline-quote) stream)
  (with-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node block-quote) stream)
  (with-block-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node document-link) stream))

(defmethod emit ((node web-link) stream))

(defmethod emit ((node list-item) stream)
  (with-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node definition) stream)
  (with-tag (nil stream :name "term")
    (emit-list (term node) stream))
  (with-tag (nil stream :name "def")
    (emit-list (definition node) stream)))

(defmethod emit ((node unordered-list) stream)
  (with-block-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node ordered-list) stream)
  (with-block-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node definition-list) stream)
  (with-block-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node image) stream)
  (with-tag (node stream :extra-attrs (list (cons "src" (source node))))
    t))

(defmethod emit ((node figure) stream))

(defmethod emit ((node table) stream)
  (with-block-tag (node stream)
    (emit-list (rows node) stream)))

(defmethod emit ((node row) stream)
  (with-block-tag (node stream)
    (emit-list (cells node) stream)))

(defmethod emit ((node cell) stream)
  (with-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node section) stream)
  (with-block-tag (node stream)
    (with-tag (nil stream :name "title")
      (emit-list (title node) stream))
    (emit-children node stream)))

(defmethod emit ((doc document) stream)
  (with-tag (nil stream :name "title")
    (write-string (title doc) stream))
  (emit-children doc stream))
