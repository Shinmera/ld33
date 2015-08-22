(in-package #:cl-user)
(asdf:defsystem ld33
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :homepage "https://github.com/Shinmera/ld33"
  :version "0.0.0"
  :components ((:file "package")
               (:file "toolkit")
               (:file "fixed-queue")
               (:file "world")
               (:file "city")
               (:file "player")
               (:file "windowing"))
  :depends-on (:flare
               :qtools
               :qtcore
               :qtgui
               :qtopengl))
