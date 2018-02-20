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
  (:method list ((generator fundamental-docstring-formatter)
                 (node fundamental-type-node))
    '(:description :arguments :returns :examples
      :side-effects :exceptional-situations :notes))
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

(defclass basic-docstring-formatter (fundamental-docstring-formatter)
  ())


(let ((node (make-instance 'fundamental-type-node)))
  (defmethod type-node-instance ((generator basic-docstring-formatter)
                                 (type symbol))
    node))


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


(define-stream-visitors
    (output stream)
    (generator basic-docstring-formatter)
    node label data before
  ((fundamental-type-node :side-effects string)
   (format output "Side Effects:~% ~a" data))
  ((fundamental-type-node :side-effects list)
   (format output "Side Effects:~%~{ * ~a~%~}" data))
  ((fundamental-type-node :arguments list)
   (format output "Arguments:~%")
   (loop for d on data
         do (destructuring-bind (arg desc) (first d)
              (format output " ~a: ~a" arg desc)
              (unless (endp (rest d))
                (format output "~%")))))
  ((fundamental-type-node :returns string)
   (format output "Returns:~% ~a" data))
  ((fundamental-type-node :returns list)
   (format output "Returns:~%~{ * ~a~^~%~}" data))
  ((fundamental-type-node :notes string)
   (format output "Note:~% ~a" data))
  ((fundamental-type-node :notes list)
   (format output "Notes:~%~{ * ~a~^~%~}" data))
  ((fundamental-type-node :description string)
   (format output "~a" data))
  ((fundamental-type-node :examples list)
   (format output "Examples:~%~{ ~a~^~%~^~%~}" data))
  ((fundamental-type-node :examples string)
   (format output "Example:~% ~a" data))
  ((fundamental-type-node :exceptional-situations list)
   (format output "Exceptional Situations:~%~{ * ~a~^~%~}" data))
  ((fundamental-type-node :exceptional-situations string)
   (format output "Exceptional Situations:~% ~a" data)))
