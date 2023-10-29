(in-package #:mana-break)

(declaim (type single-float *wood* *ore* *mana*))
(define-global-parameter *wood* 0.0)
(define-global-parameter *ore* 0.0)
(define-global-parameter *mana* 5.0)

(defun make-colonist (sprite name &key x y (speed 20.0))
  (let ((entity (make-sprite-entity sprite (float x) (float y))))
    (make-character entity :nickname name :speed (float speed))
    entity))

(defun make-colonists ()
  (make-idle-behaviour-tree
   (make-colonist "healer" "Croaker" :x 800 :y 448)))
