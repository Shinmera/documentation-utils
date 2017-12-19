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
    (apply #'append (reverse (call-next-method)))))


(defgeneric visit-one (output data generator label node before)
  (:method ((output stream)
            (data (eql nil))
            (generator fundamental-docstring-formatter)
            (label symbol)
            (node fundamental-type-node)
            (before list))
    nil))


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
    (let ((order (visiting-order generator node))
          (before nil))
      (dolist (next order)
        (let ((data (getf form next)))
          (visit-one output
                     data
                     generator
                     next
                     node
                     before)
          (unless (null data)
            (push next before)))))
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


(defclass compiler-macro-node (fundamental-type-node)
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

#|
Default formatting.
|#

(defclass default-docstring-formatter (basic-docstring-formatter)
  ())


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node variable-node))
  '(:initial-value :description :examples :notes))


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node package-node))
  '(:description :notes))


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node operator-node))
  '(:arguments :description :examples))


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node function-node))
  '(:returns :exceptional-situations :side-effects :notes))


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node type-node))
  '(:description :notes))


(defmethod build-visiting-order list
    ((generator default-docstring-formatter)
     (node compiler-macro-node))
  '(:description :notes))


(defmacro define-stream-visitors 
  ((output output-class) (generator generator-class)
   node label data before
   &body body)
  `(progn
     ,@(mapcar (lambda (x)
                 (destructuring-bind ((node-class label-symbol d-class) . body) x
                   `(defmethod visit-one ((,output ,output-class)
                                          (,data ,d-class)
                                          (,generator ,generator-class)
                                          (,label (eql ,label-symbol))
                                          (,node ,node-class)
                                          (,before list))
                      (unless (endp ,before)
                        (format output "~%~%"))
                      ,@body)))
               body)))


(define-stream-visitors (output stream) (generator default-docstring-formatter) node label data before
  ((function-node :side-effects string) (format output "Side Effects:~% ~a" data))
  ((function-node :side-effects list) (format output "Side Effects:~%~{ * ~a~%~}" data))
  ((function-node :side-effects (eql nil)) (format output "No side effects."))
  ((function-node :arguments list)
   (format output "Arguments:~%")
   (loop for d on data
         do (destructuring-bind (arg desc) (first d)
              (format output " ~a: ~a" arg desc)
              (unless (endp (rest d))
                (format output "~%")))))
  ((function-node :returns string) (format output "Returns:~% ~a" data))
  ((function-node :returns list) (format output "Returns:~%~{ * ~a~^~%~}" data))
  ((fundamental-type-node :notes string) (format output "Note:~% ~a" data))
  ((fundamental-type-node :notes list) (format output "Notes:~%~{ * ~a~^~%~}" data))
  ((fundamental-type-node :description string) (format output "~a" data))
  ((fundamental-type-node :examples list) (format output "Examples:~%~{ ~a~^~%~^~%~}" data))
  ((fundamental-type-node :examples string) (format output "Example:~% ~a" data))
  ((function-node :exceptional-situations list) (format output "Exceptional Situations:~%~{ * ~a~^~%~}" data))
  ((function-node :exceptional-situations string) (format output "Exceptional Situations:~% ~a" data)))
