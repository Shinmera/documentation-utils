#|
 This file is a part of documentation-utils
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:documentation-utils
  (:nicknames #:docs #:org.shirakumo.documentation-utils)
  (:use #:cl)
  (:export
   #:*documentation-tests*
   #:*documentation-translators*
   #:basic-string-formatter
   #:build-visiting-order
   #:check
   #:class-node
   #:compiler-macro-node
   #:condition-node
   #:define-docs
   #:define-documentation-alias
   #:define-documentation-test
   #:define-documentation-translator
   #:documentation-test
   #:documentation-translator
   #:error-node
   #:function-node
   #:fundamental-docstring-formatter
   #:fundamental-type-node
   #:generic-node
   #:macro-node
   #:method-combination-node
   #:operator-node
   #:package-node
   #:record-node
   #:remove-documentation-test
   #:remove-documentation-translator
   #:struct-node
   #:type-node
   #:type-node-instance
   #:type-node-instance
   #:variable-node
   #:visit-all
   #:visit-one
   #:visiting-order))
