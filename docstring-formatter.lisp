(in-package #:org.shirakumo.documentation-utils)

#|
Full protocol of docstring formatter.
|#

(defclass fundamental-docstring-formatter ()
  ()
  (:documentation "Fundamental, protocol class of docstring formatter."))


(defclass fundamental-type-node ()
  ()
  (:documentation "Fundamental, protocol class of all type-nodes"))


(defgeneric type-node-instance (generator type)
  (:documentation "Return instance of type node corresponding to type."))


(defgeneric visiting-order (generator node)
  (:method ((generator fundamental-docstring-formatter)
            (node fundamental-type-node))
    (build-visiting-order generator node)))


(defgeneric build-visiting-order (generator node)
  (:method-combination list)
  (:method :around ((generator fundamental-docstring-formatter)
                    (node fundamental-type-node))
    (apply #'nconc (nreverse (call-next-method)))))


(defgeneric visit-one (output generator node data))


(defgeneric visit-all (output generator node form)
  (:method ((output stream)
            (generator fundamental-docstring-formatter)
            (node fundamental-type-node)
            (form string))
    (print form output))
  (:method ((output t)
            (generator fundamental-docstring-formatter)
            (node fundamental-type-node)
            (form list))
    (let ((order (visiting-order generator node)))
      (dolist (next order)
        (let ((data (getf next form)))
          (unless (null data)
            (visit-one output
                       generator
                       node
                       data))))
      output)))

#|
Basic node type hierarchy of Common Lisp.
|#

(defclass operator-node (fundamental-node)
  ())


(defclass function-node (operator-node)
  ())


(defclass macro-node (operator-node)
  ())


(defclass generic-node (function-node)
  ())


(defclass record-node (fundamental-node)
  ())


(defclass class-node (record-node)
  ())


(defclass struct-node (record-node)
  ())


(defclass condition-node (record-node)
  ())


(defclass error-node (condition-node)
  ())

#|

|#
