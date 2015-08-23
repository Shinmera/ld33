(in-package #:cl-user)
(defpackage #:ld33
  (:use #:cl+qt #:flare #:flare-vector)
  (:shadowing-import-from #:flare-vector #:copy)
  (:export
   #:main
   #:standalone))
