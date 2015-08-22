(in-package #:ld33)

(defclass fixed-size-queue ()
  ((objects :initarg :objects :accessor objects)
   (tail :initform 0 :accessor tail)
   (head :initform 0 :accessor head)))

(defmethod size ((queue fixed-size-queue))
  (if (<= (head queue) (tail queue))
      (- (tail queue) (head queue))
      (+ (tail queue) (- (length (objects queue)) (head queue)))))

(defmethod capacity ((queue fixed-size-queue))
  (length (objects queue)))

(defun make-fixed-size-queue (size)
  (make-instance 'fixed-size-queue :objects (make-array (1+ size))))

(defun enqueue (object queue)
  (setf (aref (objects queue) (tail queue)) object)
  (setf (tail queue) (mod (1+ (tail queue)) (capacity queue)))
  (when (= (head queue) (tail queue))
    (setf (head queue) (mod (1+ (head queue)) (capacity queue))))
  queue)

(defun map-queue (function queue)
  (loop for i = (head queue) then (mod (1+ i) (capacity queue))
        for el = (aref (objects queue) i)
        until (= i (tail queue))
        do (funcall function el)))

(defmacro do-queue ((element queue) &body body)
  `(block NIL (map-queue (lambda (,element) ,@body) ,queue)))
