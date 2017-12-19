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


(defgeneric visit-one (output generator node label data))


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
        (let ((data (getf form next)))
          (unless (null data)
            (visit-one output generator
                       node next data)))))
    output))

#|
Basic node type hierarchy of Common Lisp.
|#

;; TODO not everything is handled in specific way, because currently we are using symbol passed to setf documentation

(defclass operator-node (fundamental-type-node)
  ())


(defclass function-node (operator-node)
  ())


(defclass type-node (fundamental-type-node)
  ())


(defclass variable-node (fundamental-type-node)
  ())


(defclass method-combination-node (fundamental-type-node)
  ())


(defclass compiler-macro-node (operator-node)
  ())


(defclass macro-node (operator-node)
  ())


(defclass generic-node (function-node)
  ())


(defclass record-node (type-node)
  ())


(defclass class-node (record-node)
  ())


(defclass struct-node (record-node)
  ())


(defclass condition-node (record-node)
  ())


(defclass error-node (condition-node)
  ())


(defclass package-node (fundamental-type-node)
  ())

#|
Basic stuff for handling standard lisp.
|#

(defclass basic-docstring-formatter (fundamental-docstring-formatter)
  ())


(let ((function-node (make-instance 'function-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'function)))
     function-node))


(let ((generic-node (make-instance 'generic-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'generic)))
    generic-node))


(let ((type-node (make-instance 'type-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'type)))
    type-node))


(let ((variable-node (make-instance 'variable-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'variable-node)))
    variable-node))


(let ((method-combination-node (make-instance 'method-combination-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'method-combination-node)))
    method-combination-node))


(let ((compiler-macro (make-instance 'compiler-macro-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'compile-macro-node)))
    compiler-macro))


(let ((package-node (make-instance 'package-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type (eql 'package-node)))
    package-node))
