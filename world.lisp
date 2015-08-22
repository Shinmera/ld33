(in-package #:ld33)
(in-readtable :qtools)

(defmethod finalize :before ((container container))
  (flare-indexed-set:do-set (i obj) (objects container)
    (declare (ignore i))
    (finalize obj)))

(defclass world (scene)
  ((extent :initform '(-512 +512 -512 +512) :accessor extent)
   (timers :initform (make-hash-table :test 'eql) :accessor timers)))

(defmethod initialize-instance :after ((world world) &key)
  (enter (make-instance 'origin) world))

(defmethod update ((world world))
  (call-next-method)
  (flare-indexed-set:do-set (i obj) (objects world)
    (declare (ignore i))
    (update obj)))

(defmethod cap ((world world) vec)
  (destructuring-bind (left right bottom top) (extent world)
    (setf (x vec) (max left (min right (x vec))))
    (setf (y vec) (max bottom (min top (y vec)))))
  vec)

(defmethod paint ((world world) target)
  (call-next-method)
  (q+:fill-rect target -3000 +512 6000 1024 (q+:qt.black)))

(defmethod timer-ready-p (name timeout (world world))
  (let ((last (gethash name (timers world))))
    (cond ((not last)
           (setf (gethash name (timers world)) (clock world))
           NIL)
          ((< (+ last timeout) (clock world))
           (setf (gethash name (timers world)) (clock world))))))

(defmacro with-timer-ready ((name timeout &optional (world '(world))) &body body)
  `(when (timer-ready-p ,name ,timeout ,world)
     ,@body))

(defclass origin (entity)
  ())

(defmethod name ((origin origin))
  :origin)

(defmethod paint ((origin origin) target)
  (call-next-method)
  (setf (q+:color (q+:brush target)) (q+:qt.white))
  (q+:draw-ellipse target
                   (round (- (x (location origin)) 10))
                   (round (- (y (location origin)) 10))
                   20 20))

(defclass sprite-entity (entity)
  ((sprite :initarg :sprite :accessor sprite)))

(defmethod initialize-instance :after ((entity sprite-entity) &key)
  (setf (sprite entity) (sprite entity)))

(defmethod finalize :after ((entity sprite-entity))
  (finalize (sprite entity)))

(defmethod (setf sprite) (sprite (entity sprite-entity))
  (etypecase sprite
    (string (setf (sprite entity) (q+:make-qimage (asset sprite))))
    (pathname (setf (sprite entity) (q+:make-qimage (uiop:native-namestring sprite))))
    (qobject
     (unless (qtypep sprite "QImage")
       (error "~s is not of class QImage." sprite))
     (setf (slot-value entity 'sprite) sprite))))

(defun draw-image-centered (image target)
  (q+:draw-image target (round (/ (q+:width image) -2)) (round (/ (q+:height image) -2)) image))

(defmethod paint ((entity sprite-entity) target)
  (with-translation ((location entity) target)
    (draw-image-centered (sprite entity) target)))

(defmethod leave :after ((entity sprite-entity) from)
  (finalize entity))

(defclass hitbox-entity (entity)
  ((w :initarg :w :accessor w)
   (h :initarg :h :accessor h))
  (:default-initargs
   :w 10 :h 10))

(defmethod colliding ((entity hitbox-entity) (point vec))
  (v:info :test "E: ~a P: ~a" (location entity) point)
  (and (<= (- (x (location entity)) (/ (w entity) 2))
           (x point)
           (+ (x (location entity)) (/ (w entity) 2)))
       (<= (- (y (location entity)) (/ (h entity) 2))
           (y point)
           (+ (y (location entity)) (/ (h entity) 2)))))

(defmethod colliding ((entity hitbox-entity) (other entity))
  (colliding entity (location other)))

(defmethod colliding ((entity hitbox-entity) (other hitbox-entity))
  (or (colliding entity (vec (- (x (location other)) (/ (w other) 2))
                             (- (y (location other)) (/ (h other) 2))
                             0))
      (colliding entity (vec (+ (x (location other)) (/ (w other) 2))
                             (- (y (location other)) (/ (h other) 2))
                             0))
      (colliding entity (vec (+ (x (location other)) (/ (w other) 2))
                             (+ (y (location other)) (/ (h other) 2))
                             0))
      (colliding entity (vec (- (x (location other)) (/ (w other) 2))
                             (+ (y (location other)) (/ (h other) 2))
                             0))
      (colliding other (vec (- (x (location entity)) (/ (w entity) 2))
                             (- (y (location entity)) (/ (h entity) 2))
                             0))
      (colliding other (vec (+ (x (location entity)) (/ (w entity) 2))
                             (- (y (location entity)) (/ (h entity) 2))
                             0))
      (colliding other (vec (+ (x (location entity)) (/ (w entity) 2))
                             (+ (y (location entity)) (/ (h entity) 2))
                             0))
      (colliding other (vec (- (x (location entity)) (/ (w entity) 2))
                             (+ (y (location entity)) (/ (h entity) 2))
                             0))))

(defclass bullet (particle sprite-entity hitbox-entity)
  ()
  (:default-initargs
   :sprite "graphics/part1.png"))

(defmethod paint ((bullet bullet) target)
  (call-next-method))

(defclass damageable-entity (entity)
  ((health :initarg :health :accessor health))
  (:default-initargs
   :health 100))

(defmethod paint :after ((entity damageable-entity) target)
  (q+:fill-rect target
                (- (x (location entity)) (health entity))
                (- (y (location entity)) 2)
                (* (health entity) 2)
                2
                (q+:qt.white)))

(defmethod damage ((entity damageable-entity) amount)
  (decf (health entity) amount)
  (when (<= (health entity) 0)
    (leave entity T)))

(defclass enemy (entity)
  ())
