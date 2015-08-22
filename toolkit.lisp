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
