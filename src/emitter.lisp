(in-package :cl-user)
(defpackage scriba.emitter
  (:use :cl :common-doc)
  (:export :emit
           :emit-to-string)
  (:documentation "Dump CommonDoc nodes to Scriba text."))
(in-package :scriba.emitter)

;;; Utilities

(defmacro with-tag-name ((name stream) &body body)
  "Emit a tag name."
  `(progn
     (write-char #\@ ,stream)
     (write-string ,name ,stream)
     ,@body))

(defun emit-hash-table (hash-table stream &optional (delimiter #\Space))
  "Emit a hash table."
  (flet ((string-needs-quoting-p (string)
           (when (or (position #\" string)
                     (position delimiter string))
             t)))
    (when (> (hash-table-count hash-table) 0)
      (write-char #\[ stream)
      (loop for attr-name being the hash-keys of hash-table
            for attr-value being the hash-values of hash-table
            for idx downfrom (1- (hash-table-count hash-table))
            for last-item = (zerop idx)
            do (write-string attr-name stream)
               (write-char #\= stream)
               (if (string-needs-quoting-p attr-value)
                   (prin1 attr-value stream)
                   (princ attr-value stream))
            unless last-item
              do (write-char delimiter stream))
      (write-char #\] stream))))

(defmacro with-tag-attrs ((attrs stream &key extra) &body body)
  "Emit tag attributes."
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
  "Emit a tag body."
  `(progn
     (write-char #\( ,stream)
     ,@body
     (write-char #\) ,stream)))

(defmacro with-tag ((node stream &key name extra-attrs) &body body)
  "Emit a whole tag."
  (let ((name (if name
                  name
                  `(find-tag (class-of ,node)))))
    `(with-tag-name (,name ,stream)
       (with-tag-attrs ((if ,node (metadata ,node) nil) ,stream :extra ,extra-attrs)
         (with-tag-body (,stream)
           ,@body)))))

(defmacro with-block-tag ((node stream &key name extra-attrs) &body body)
  "Emit a block tag."
  (let ((name (if name
                  name
                  `(find-tag (class-of ,node)))))
    `(progn
       (with-tag-name ("begin" ,stream)
         (with-tag-attrs ((if ,node (metadata ,node) nil) ,stream :extra ,extra-attrs)
           (with-tag-body (,stream)
             (write-string ,name ,stream)))
         (write-char #\Newline ,stream))
       ,@body
       (format ,stream "~&")
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
  (let* ((lang (language node))
         (extra-args (when (and lang
                                (not (string= lang "")))
                       (list (cons "lang" lang)))))
    (with-block-tag (node stream :extra-attrs extra-args)
      (emit-children node stream))))

(defmethod emit ((node inline-quote) stream)
  (with-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node block-quote) stream)
  (with-block-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node document-link) stream)
  (with-tag (node stream
             ;; COMMON-DOC-PLUMP.PARSER:PARSE function does not
             ;; put into the metadata ID attribute.
             ;; That is why we need to pass id as an extra argument:
             :extra-attrs (list (cons "id"
                                      (node-reference
                                       node))))
    (emit-children node stream)))

(defmethod emit ((node web-link) stream)
  (with-tag (node stream)
    (emit-children node stream)))

(defmethod emit ((node list-item) stream)
  (with-tag (node stream)
    (emit-children node stream))
  (write-char #\Newline stream))

(defmethod emit ((node definition) stream)
  (with-tag (nil stream :name "term")
    (emit-list (term node) stream))
  (write-char #\Newline stream)
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
    (write-char #\Newline stream)
    (write-char #\Newline stream)
    (emit-children node stream)))

(defmethod emit ((doc document) stream)
  (with-tag (nil stream :name "title")
    (write-string (title doc) stream))
  (write-char #\Newline stream)
  (write-char #\Newline stream)
  (emit-children doc stream)
  (write-char #\Newline stream))

(defun emit-to-string (node-or-doc)
  (with-output-to-string (stream)
    (emit node-or-doc stream)))
