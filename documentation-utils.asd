(asdf:defsystem documentation-utils
  :version "1.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A few simple tools to help you with documenting your library."
  :homepage "https://shinmera.com/docs/documentation-utils/"
  :bug-tracker "https://shinmera.com/project/documentation-utils/issues"
  :source-control (:git "https://shinmera.com/project/documentation-utils.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "documentation"))
  :depends-on (:trivial-indent))
