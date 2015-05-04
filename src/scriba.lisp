(in-package :cl-user)
(defpackage scriba
  (:use :cl)
  (:import-from :common-doc.format
                :document-format
                :parse-document
                :emit-document)
  (:export :scriba)
  (:documentation "The main CommonDoc interface."))
(in-package :scriba)

(defun plump-to-text (plump-node stream)
  (if (typep plump-node 'plump:text-node)
      (write-string (plump:text plump-node) stream)
      (loop for child across (plump:children plump-node) do
        (plump-to-text child stream))))

(defun string-to-common-doc (string)
  (let ((common-doc-plump.parser:*serializer* #'plump-to-text))
    (common-doc-plump.parser:parse-document
     (scriba.plump:parse string))))

(defclass scriba (document-format)
  ()
  (:documentation "The VerTeX format."))

(defmethod parse-document ((scriba scriba)
                           (string string))
  "Return a VerTeX document parsed from a string."
  (declare (ignore scriba))
  (string-to-common-doc string))

(defmethod parse-document ((scriba scriba)
                           (pathname pathname))
  "Return a VerTeX document parsed from a file."
  (declare (ignore scriba))
  (string-to-common-doc (uiop:read-file-string pathname)))

(defmethod emit-document ((scriba scriba)
                          (node common-doc:document-node)
                          stream)
  (declare (ignore scriba))
  (scriba.emitter:emit node stream))

(defmethod emit-document ((scriba scriba)
                          (doc common-doc:document)
                          stream)
  (declare (ignore scriba))
  (scriba.emitter:emit doc stream))
