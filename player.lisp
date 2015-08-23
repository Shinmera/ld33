(in-package #:ld33)
(in-readtable :qtools)

(defun make-tail (size)
  (let ((tail ()))
    (push (make-instance 'player-part :sprite "graphics/snaketail.png") tail)
    (dotimes (i size)
      (push (make-instance 'player-part) tail))
    tail))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass player (sprite-entity hitbox-entity)
    ((v :initform (vec 0 0 0) :accessor v)
     (vmax :initform 15 :accessor vmax)
     (vacc :initform 2 :accessor vacc)
     (vdcc :initform 0.65 :accessor vdcc)
     (parts :initform (make-tail 5) :accessor parts)
     (positions :initform (make-fixed-size-queue 10))
     (angle :initform 0 :accessor angle)
     (invincible :initform NIL :accessor invincible)
     (power :initform 2 :accessor power))
    (:default-initargs
     :sprite "graphics/snakehead.png")))

(defclass player-part (sprite-entity)
  ((angle :initform 0 :accessor angle))
  (:default-initargs
   :sprite "graphics/snakebody.png"))

(defmethod paint ((part player-part) target)
  (with-translation ((location part) target)
    (q+:rotate target (angle part))
    (draw-image-centered (sprite part) target)))

(defun distribute-parts (positions parts)
  (let ((lengths ())
        (length 0))
    (when (<= 2 (size positions))
      ;; calculate lengths
      (let ((prev NIL))
        (do-queue (pos positions)
          (let ((a (car pos))
                (pos (cdr pos)))
            (if prev
                (let ((l (size (-translated pos prev))))
                  (when (< 0 l)
                    (incf length l)
                    (push (list prev pos l a) lengths)
                    (setf prev pos)))
                (setf prev pos)))))
      ;; FIXME: Somehow this is fucking broken but I don't know why or how.
      ;; extrapolate leftover tail
      (let ((leftover (- (* (length parts) 20) length)))
        (when (and (< 0 leftover) lengths)
          (destructuring-bind (p n l a) (car lengths)
            (let ((diff (-translated n p)))
              (push (list
                     n
                     (translate (scale diff (- (/ leftover (- (size diff))))) n)
                     leftover
                     a)
                    lengths)))
          (incf length leftover)))
      (setf lengths (nreverse lengths))
      ;; step
      (when lengths
        (let* ((step (/ length (length parts))))
          (loop with dist = (/ step 2)
                for (p n l a) in lengths
                for left = 0 then right
                for right = l then (+ right l)
                while parts
                do (loop while (and parts (<= left dist right))
                         do (let ((part (pop parts)))
                              (setf (location part)
                                    (translate
                                     (scale
                                      (-translated n p)
                                      (/ (- dist left) l))
                                     p))
                              (setf (angle part) a))
                            (incf dist step))))))))

(defmethod update ((player player))
  (call-next-method)
  (with-slots-bound (player player)
    (let ((motion-input NIL))
      (when (key-pressed-p (q+:qt.key_left))
        (decf (x v) vacc)
        (setf motion-input T))
      (when (key-pressed-p (q+:qt.key_right))
        (incf (x v) vacc)
        (setf motion-input T))
      (when (key-pressed-p (q+:qt.key_up))
        (incf (y v) vacc)
        (setf motion-input T))
      (when (key-pressed-p (q+:qt.key_down))
        (decf (y v) vacc)
        (setf motion-input T))

      (cond ((<= (size v) vdcc)
             (scale v 0))
            ((not motion-input)
             (scale v vdcc))))
    
    (when (<= vmax (size v))
      (scale v (/ vmax (size v))))
    
    (unless (vec= v (vec 0 0 0))
      (setf angle (+ 180 (/ (atan (x v) (y v)) PI 1/180)))
      (enqueue (cons angle (copy (location player))) positions))
    (distribute-parts positions parts)

    (when invincible
      (with-timer-ready ('invincible 3)
        (timer-ready-p 'invincible 3 (world))
        (setf invincible NIL)))

    (do-container-tree (part (world))
      (when (and (typep part 'planet)
                 (colliding part (location player))
                 (colliding part (vec (+ (x (location player)) (x v))
                                      (- (y (location player)) (y v))
                                      0)))
        (let* ((n (normalize (-translated (location player) (location part))))
               (pos (translated (scaled n (1+ (size part))) (location part))))
          (setf (location player) pos)
          (-translate v (scale n (* 2 (dot n v)))))
        (enter (make-instance 'explosion
                              :size (+ 50 (random 150))
                              :location (copy (location player)))
               player)
        (damage part (power player)))
      (when (and (typep part 'planet-bullet)
                 (not invincible)
                 (colliding part player))
        (cond ((cdr parts)
               (pop parts))
              (T
               (leave player T)
               (stop (world))))
        (setf invincible T)))

    (incf (x (location player)) (x v))
    (decf (y (location player)) (y v))
    (cap (world) (location player))))

(defmethod paint ((player player) target)
  (flet ((paint ()
           (dolist (part (parts player))
             (paint part target))
           (with-translation ((location player) target)
             (q+:rotate target (angle player))
             (draw-image-centered (sprite player) target))))
    (if (invincible player)
        (unless (timer-ready-p 'invincible-blink 0.1 (world))
          (paint))
        (paint)))
  (flare-indexed-set:do-set (i obj) (objects player)
    (declare (ignore i))
    (paint obj target)))
