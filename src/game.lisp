(in-package #:mana-break)

(declaim (type single-float *wood* *ore* *mana*))
(define-global-parameter *wood* 0.0)
(define-global-parameter *ore* 0.0)
(define-global-parameter *mana* 0.0)

(defun make-colonist (name &key x y (speed 100.0))
  (let ((entity (make-sprite-entity name (float x) (float y))))
    (make-character entity :speed (float speed))
    entity))

(defun make-colonists ()
  (make-idle-behaviour-tree
   (make-colonist "ordinary human" :x 800 :y 448)))
