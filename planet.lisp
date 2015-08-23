(in-package #:ld33)
(in-readtable :qtools)

(defclass planet (sprite-entity damageable-entity sized-entity enemy)
  ((angle :initform 0 :accessor angle)
   (sight :initarg :sight :accessor sight)
   (rotation :initarg :rotation :accessor rotation)))

(defmethod paint ((planet planet) target)
  (with-translation ((location planet) target)
    (q+:rotate target (angle planet))
    (draw-image-centered (sprite planet) target))
  (flare-indexed-set:do-set (i val) (objects planet)
    (paint val target)))

(defmethod update ((planet planet))
  (call-next-method)
  (setf (angle planet) (mod (+ (angle planet) (rotation planet)) 360))
  (let ((progression (progression (type-of planet) (world))))
    (when (and (< (size (-translated (location (player))
                                     (location planet)))
                  (sight planet))
               (not (running progression)))
      (stop progression)
      (reset progression)
      (start progression)))
  (when (< (health planet) 0)
    (decf (visibility planet) 0.1))
  (when (< (visibility planet) 0)
    (leave planet T)
    ;; Stop if nothing left.
    (or (do-container-tree (object (world))
          (when (typep object 'planet)
            (return T)))
        (stop (world)))))

(defmethod enter :after ((planet planet) (world world))
  (enter (progression-definition (type-of planet)) world))

(defclass planet-bullet (bullet)
  ()
  (:default-initargs
   :sprite "graphics/part1.png"))

(defclass planet1 (planet)
  ()
  (:default-initargs
   :name :planet1
   :location (vec 0 1024 0)
   :size 128
   :sight 800
   :rotation 0.2
   :sprite "graphics/planet1.png"))

(define-progression planet1
  0 0 (:planet1 (enter ring :name :ring1 :size 128 :contents (planet-bullet :count 10)
                            :location (location (unit :planet1 (world)))))
  0 8 ((:planet1 :ring1) (increase angle :by 2))
  0 5 ((:planet1 :ring1) (set! size :to 600 :ease 'quad-in-out))
  2 2 (:planet1 (enter ring :name :ring2 :size 128 :contents (planet-bullet :count 15)
                            :location (location (unit :planet1 (world)))))
  2 10 ((:planet1 :ring2) (increase angle :by -1))
  2 8 ((:planet1 :ring2) (set! size :to 300 :ease 'quad-in-out))
  5 8 ((:planet1 :ring1) (set! size :to 400 :ease 'quad-in-out))
  8 10 ((:planet1 >) (set! size :to 128 :ease 'quad-in-out))
  10 ((:planet1 >) (leave))
  10)

(defclass planet2 (planet)
  ()
  (:default-initargs
   :name :planet2
   :location (vec 512 -1024 0)
   :size 64
   :sight 500
   :rotation 0.4
   :health 50
   :sprite "graphics/planet2.png"))

(define-progression planet2
  0 0 (:planet2 (enter ring :name :ring1 :size 64 :contents (planet-bullet :count 5)
                            :location (location (unit :planet2 (world))))
                (enter ring :name :ring2 :size 64 :contents (planet-bullet :count 5)
                            :location (location (unit :planet2 (world))))
                (enter ring :name :ring3 :size 64 :contents (planet-bullet :count 5)
                            :location (location (unit :planet2 (world)))))
  0 30 ((:planet2 :ring1) (increase angle :by 2))
  0 30 ((:planet2 :ring2) (increase angle :by -2))
  0 30 ((:planet2 :ring3) (increase angle :by 2))
  0 10 ((:planet2 :ring1)
        (set! size :to 500 :ease 'circ-in))
  0 20 ((:planet2 :ring2)
        (set! size :to 300 :ease 'quad-in-out))
  0 20 ((:planet2 :ring3)
        (increase angle :by 2)
        (set! size :to 200 :ease 'circ-out))
  10 15 ((:planet2 :ring1)
         (set! size :to 100 :ease 'quad-in-out))
  15 20 ((:planet2 :ring1)
         (set! size :to 400 :ease 'circ-out))
  20 30 ((:planet2 >)
         (set! size :to 64 :ease 'expo-in))
  30 30 ((:planet2 >) (leave)))

(defclass planet3 (planet)
  ()
  (:default-initargs
   :name :planet3
   :location (vec -800 -1200 0)
   :size 320
   :sight 1500
   :rotation 0.3
   :health 200
   :sprite "graphics/planet3.png"))

(define-progression planet3
  0 0 (:planet3 (enter ring :name :ground :size 330 :contents (planet-bullet :count 5)
                            :location (location (unit :planet3 (world))))
                (enter ring :name :attack1 :size 320 :contents (planet-bullet :count 10)
                            :location (location (unit :planet3 (world)))))
  0 60 ((:planet3 :ground) (increase angle :by 3))
  0 5 ((:planet3 :attack1) (increase angle :by 4))
  0 10 ((:planet3 :attack1) (set! size :to 800 :ease 'quad-in-out))
  5 15 ((:planet3 :attack1) (increase angle :by 5))
  10 15 ((:planet3 :attack1) (set! visibility :to 0 :ease 'linear))
  15 15 ((:planet3 :attack1) (leave))
  16 16 (:planet3 (enter ring :name :attack2 :size 320 :contents (planet-bullet :count 7)
                              :location (location (unit :planet3 (world)))))
  16 20 ((:planet3 :attack2) (set! size :to 600 :ease 'quad-in-out))
  16 20 ((:planet3 :attack2) (set! angle :to -180 :ease 'quart-in))
  20 60 ((:planet3 :attack2) (increase angle :by -3))
  20 20 (:planet3 (enter ring :name :attack3 :size 320 :contents (ring :size 20 :count 5
                                                                       :contents (planet-bullet :count 2))
                              :location (location (unit :planet3 (world)))))
  20 50 ((:planet3 :attack3) (increase angle :by 2))
  20 50 ((:planet3 :attack3 >) (increase angle :by 3))
  20 25 ((:planet3 :attack3) (set! size :to 500 :ease 'quad-in-out))
  25 30 ((:planet3 :attack3 >) (set! size :to 100 :ease 'quad-in-out))
  30 40 ((:planet3 :attack3) (set! size :to 800 :ease 'quad-in-out))
  40 50 ((:planet3 :attack3) (set! size :to 320 :ease 'quad-in-out))
  50 50 ((:planet3 :attack3) (leave))
  50 60 ((:planet3 :attack2) (set! size :to 320 :ease 'expo-in))
  60 60 ((:planet3 >) (leave)))
