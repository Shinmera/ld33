(in-package #:ld33)
(in-readtable :qtools)

(defun make-tail (size)
  (let ((tail ()))
    (push (make-instance 'player-part :sprite "graphics/snaketail.png") tail)
    (dotimes (i size)
      (push (make-instance 'player-part) tail))
    (nreverse tail)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass player (sprite-entity)
    ((v :initform (vec 0 0 0) :accessor v)
     (vmax :initform 15 :accessor vmax)
     (vacc :initform 1 :accessor vacc)
     (vdcc :initform 0.65 :accessor vdcc)
     (parts :initform (make-tail 5) :accessor parts)
     (positions :initform (make-fixed-size-queue 10))
     (angle :initform 0 :accessor angle))
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

(defclass player-bullet (bullet)
  ((v :initarg :v :accessor v))
  (:default-initargs
   :sprite "graphics/part1.png"))

(defmethod update ((bullet player-bullet))
  (incf (x (location bullet)) (x (v bullet)))
  (incf (y (location bullet)) (y (v bullet)))
  ;; collision, horrible kludge. Forgive me, future me.
  (do-container-tree (entity (world))
    (when (and (typep entity 'enemy)
               (colliding bullet entity))
      (damage entity 1)
      (leave bullet (player))
      (return)))
  ;; death
  (with-timer-ready (bullet 2)
    (leave bullet (player))))

(defmethod paint ((bullet player-bullet) target)
  (call-next-method))

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
      ;; (let ((leftover (- (* (length parts) 20) length)))
      ;;   (when (and (< 0 leftover) lengths)
      ;;     (destructuring-bind (p n l a) (car lengths)
      ;;       (let ((diff (-translated n p)))
      ;;         (push (list
      ;;                n
      ;;                (translate (scale diff (/ leftover (- (size diff)))) n)
      ;;                leftover
      ;;                a)
      ;;               lengths)))
      ;;     (incf length leftover)))
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
  (flare-indexed-set:do-set (i obj) (objects player)
    (declare (ignore i))
    (update obj))
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

    (when (key-pressed-p (q+:qt.key_space))
      (with-timer-ready ('bullet 0.1)
        (enter (make-instance 'player-bullet :location (copy (location player))
                                             :v (scale (deg->xy (+ 90 angle)) 20)) player)))

    (incf (x (location player)) (x v))
    (decf (y (location player)) (y v))
    (cap (scene player) (location player))))

(defmethod paint ((player player) target)
  (flare-indexed-set:do-set (i obj) (objects player)
    (declare (ignore i))
    (paint obj target))
  (dolist (part (parts player))
    (paint part target))
  (with-translation ((location player) target)
    (q+:rotate target (angle player))
    (draw-image-centered (sprite player) target)))
