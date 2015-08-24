(in-package #:ld33)
(in-readtable :qtools)

(defvar *main* NIL)
(defparameter *fps* 1000/30)

(define-widget main (QGLWidget)
  ((world :initform (make-instance 'world) :finalized T)
   (player :initform (make-instance 'player :name :player))
   (keys :initform (make-hash-table :test 'eql) :accessor keys)))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (asset 'image "graphics/bg.png"))

(define-initializer (main setup)
  (setf *main* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) "The End of All We Know")
  (enter (make-instance 'planet1) world)
  (enter (make-instance 'planet2) world)
  (enter (make-instance 'planet3) world)
  (enter player world))

(define-finalizer (main teardown)
  (loop for v being the hash-values of *asset-store* do (finalize v))
  (setf *asset-store* (make-hash-table :test 'equalp))
  (setf *main* NIL))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (with-simple-restart (abort "Abort the update and continue.")
      (update world))
    (q+:repaint main)
    (let* ((elapsed (* (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)
                       1000)))
      (q+:start timer (round (max 0 (- *fps* elapsed)))))))

(defmethod stop ((main main))
  (stop (slot-value main 'world)))

(defmethod start ((main main))
  (start (slot-value main 'world)))

(defmethod running ((main main))
  (running (slot-value main 'world)))

(defun press-key (key &optional (main *main*))
  (setf (gethash key (keys main)) key))

(defun release-key (key &optional (main *main*))
  (remhash key (keys main)))

(defun key-pressed-p (key &optional (main *main*))
  (gethash key (keys main)))

(defun world (&optional (main *main*))
  (slot-value main 'world))

(defun player (&optional (main *main*))
  (slot-value main 'player))

(define-override (main key-release-event) (ev)
  (release-key (q+:key ev) main)
  (stop-overriding))

(define-override (main key-press-event) (ev)
  (press-key (q+:key ev) main)
  (case (q+:key ev)
    (#.(q+:qt.key_escape)
     (if (running main)
         (stop main)
         (start main))))
  (stop-overriding))

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort the drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
      (setf (q+:render-hint painter) (q+:qpainter.text-antialiasing))
      (setf (q+:render-hint painter) (q+:qpainter.smooth-pixmap-transform))
      (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
      (setf (q+:style (q+:background painter)) (q+:qt.solid-pattern))
      (setf (q+:color (q+:background painter)) (q+:qt.black))
      (setf (q+:style (q+:brush painter)) (q+:qt.solid-pattern))
      ;; background
      (setf (q+:transform bgbrush)
            (q+:translate (q+:transform bgbrush)
                          (* (x (location player)) -2)
                          (* (y (location player)) -2)))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      ;; translate view
      (let ((view (translate (vec (/ (q+:width main) 2)
                                  (/ (q+:height main) 2)
                                  0)
                             (scaled (location player) -1))))
        (with-translation (view painter)
          (paint world painter)))
      ;; overlay
      (unless (running main)
        (with-finalizing ((overlay (q+:make-qcolor 0 0 0 180))
                          (white (q+:make-qcolor 255 255 255)))
          (q+:fill-rect painter (q+:rect main) overlay)
          (setf (q+:color (q+:pen painter)) white)
          (let ((font (q+:font painter)))
            (setf (q+:font painter) font)

            (q+:draw-text
             painter
             (q+:rect main)
             (q+:qt.align-center)
             (cond ((not (unit :player world))
                    (setf (q+:point-size font) 72)
                    "Game Over")
                   ((not (do-container-tree (entity world)
                           (when (typep entity 'planet)
                             (return T))))
                    (setf (q+:point-size font) 72)
                    "Victory!")
                   (T
                    (setf (q+:point-size font) 32)
                    (format NIL "Paused.~%Escape to Resume"))))))))))

(defmethod call-with-translation (func target vec)
  (q+:save target)
  (q+:translate target (x vec) (y vec))
  (unwind-protect
       (funcall func)
    (q+:restore target)))

(defmethod paint :around ((paintable paintable) target)
  (let ((opacity (q+:opacity target)))
    (setf (q+:opacity target) (* opacity (visibility paintable)))
    (unwind-protect
         (call-next-method)
      (setf (q+:opacity target) opacity))))

(defun main (&key (blocking NIL))
  (unless *main*
    (setf v:*global-controller* (v:make-standard-global-controller))
    (with-main-window (window 'main :blocking blocking :name "The End of All We Know"))))

(defun standalone (&rest args)
  (declare (ignore args))
  #+:sbcl (sb-ext:disable-debugger)
  (setf *standalone* T)
  (main :blocking T))

