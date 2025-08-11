(asdf:defsystem multilang-documentation-utils
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Multiple-languages support for documentation-utils."
  :homepage "https://shinmera.com/docs/documentation-utils/"
  :bug-tracker "https://shinmera.com/project/documentation-utils/issues"
  :source-control (:git "https://shinmera.com/project/documentation-utils.git")
  :serial T
  :components ((:file "multilang"))
  :depends-on (:documentation-utils
               :multilang-documentation))
