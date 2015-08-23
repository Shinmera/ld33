(in-package #:ld33)
(in-readtable :qtools)

(defun deg->rad (deg)
  (* deg PI 1/180))

(defun rad->deg (rad)
  (/ rad PI 1/180))

(defun xy->deg (x y)
  (rad->deg (atan y x)))

(defun deg->xy (deg)
  (let ((rad (deg->rad deg)))
    (vec (cos rad) (sin rad) 0)))

(defvar *asset-store* (make-hash-table :test 'equalp))
(defvar *standalone* NIL)
(defun asset (type path)
  (let ((ident (format NIL "~a~a" type path)))
    (or (gethash ident *asset-store*)
        (setf (gethash ident *asset-store*)
              (let ((path (uiop:native-namestring
                           (if *standalone*
                               (merge-pathnames path (uiop:argv0))
                               (asdf:system-relative-pathname :ld33 path)))))
                (ecase type
                  (image (q+:make-qimage path))))))))
