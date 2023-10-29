(in-package #:mana-break)


(declaim
 (ftype (function (string single-float single-float)) make-sprite-entity))
(defun make-sprite-entity (name x y)
  (let ((prefab (frame-prefab :name name :sequence-number 0))
        (frame-count (length (frame-prefabs name)))
        (entity (ecs:make-entity)))
    (make-position entity :x x :y y)
    (replace-size entity prefab)
    (replace-frame entity prefab)
    (make-sprite entity
                 :name name
                 :frame-count frame-count)
    entity))

(ecs:defsystem render-frame
  (:components-ro (position frame)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap frame-bitmap position-x position-y 0))

(ecs:defsystem update-frame
  (:components-rw (frame sprite)
   :arguments ((:dt single-float)))
  (incf sprite-time dt)
  (when (> sprite-time frame-time)
    (multiple-value-bind (nframes rest-time)
        (floor sprite-time frame-time)
      (declare (type non-negative-fixnum nframes))
      (setf sprite-time rest-time
            sprite-current-frame (rem (+ sprite-current-frame nframes)
                                      sprite-frame-count))
      (replace-frame entity
                     (frame-prefab :name sprite-name
                                   :sequence-number sprite-current-frame)))))
