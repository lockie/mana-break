(in-package #:mana-break)


(defun make-colonist (name &key x y (speed 100.0))
  (let ((entity (make-sprite-entity name (float x) (float y))))
    (make-character entity :speed (float speed))
    entity))

(defun make-colonists ()
  (make-idle-behaviour-tree
   (make-colonist "ordinary human" :x 800 :y 448))
  )
