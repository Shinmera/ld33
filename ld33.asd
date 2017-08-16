
(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)
  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem ld33
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :homepage "https://github.com/Shinmera/ld33"
  :version "0.0.0"
  :components ((:file "package")
               (:file "toolkit")
               (:file "fixed-queue")
               (:file "world")
               (:file "planet")
               (:file "player")
               (:file "windowing"))
  :defsystem-depends-on (:qtools)
  :depends-on (:flare
               :qtools
               :qtcore
               :qtgui
               :qtopengl)  
  :build-operation "qt-program-op"
  :build-pathname "the-end"
  :entry-point "ld33:standalone")
