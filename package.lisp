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
   #:basic-docstring-formatter
   #:build-visiting-order
   #:check
   #:define-docs
   #:define-documentation-alias
   #:define-documentation-test
   #:define-documentation-translator
   #:documentation-test
   #:documentation-translator
   #:fundamental-docstring-formatter
   #:fundamental-type-node
   #:remove-documentation-test
   #:remove-documentation-translator
   #:visit-all
   #:visit-one
   #:visiting-order))
