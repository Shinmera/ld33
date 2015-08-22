(in-package #:ld33)
(in-readtable :qtools)

(defclass city (sprite-entity damageable-entity hitbox-entity enemy)
  ())

(defmethod initialize-instance :after ((city city) &key)
  (setf (w city) (q+:width (sprite city))
        (h city) (q+:height (sprite city))))

(defmethod paint ((city city) target)
  (with-translation ((vec 0 (/ (q+:height (sprite city)) -2) 0) target)
    (call-next-method)))

(defclass city1 (city)
  ()
  (:default-initargs
   :location (vec 0 512 0)
   :sprite "graphics/city1.png"))
