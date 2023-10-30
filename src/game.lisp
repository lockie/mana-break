(in-package #:mana-break)

(declaim (type single-float *wood* *ore* *mana*))
(define-global-parameter *wood* 10.0)
(define-global-parameter *ore* 0.0)
(define-global-parameter *mana* 5.0)

(defun make-colonist (sprite name &key x y (speed 20.0))
  (let ((entity (make-sprite-entity sprite (float x) (float y))))
    (make-character entity :nickname name :speed (float speed))
    entity))

(defun make-colonists ()
  (make-idle-behaviour-tree
   (make-colonist "healer" "Croaker" :x 1024 :y 448))
  (make-collect-wood-behaviour-tree
   (make-colonist "champion" "The Captain" :x 1120 :y 448))
  (make-idle-behaviour-tree
   (make-colonist "master of thieves" "The Lieutenant" :x 1152 :y 416))
  (make-idle-behaviour-tree
   (make-colonist "man" "Elmo" :x 1152 :y 480))
  (make-idle-behaviour-tree
   (make-colonist "archmage" "One-Eye" :x 1184 :y 416 :speed 30.0))
  (make-idle-behaviour-tree
   (make-colonist "wizard" "Goblin" :x 1184 :y 449 :speed 30.0))
  (make-idle-behaviour-tree
   (make-colonist "priest" "Silent" :x 1184 :y 480 :speed 25.0))
  (make-idle-behaviour-tree
   (make-colonist "desperado" "Raven" :x 1216 :y 256 :speed 35.0))
  (make-idle-behaviour-tree
   (make-colonist "slayer" "Otto" :x 1184 :y 320 :speed 30.0))
  (make-idle-behaviour-tree
   (make-colonist "brute" "Hagop" :x 1376 :y 192 :speed 30.0))
  (make-idle-behaviour-tree
   (make-colonist "bandit" "Pawnbroker" :x 1440 :y 544))
  (make-idle-behaviour-tree
   (make-colonist "ninja" "Mogaba" :x 1504 :y 832))
  (make-idle-behaviour-tree
   (make-colonist "knight" "Murgen" :x 1408 :y 576))
  (make-idle-behaviour-tree
   (make-colonist "monk" "Sleepy" :x 1440 :y 608))
  (make-idle-behaviour-tree
   (make-colonist "tourist" "Suvrin" :x 1504 :y 608)))

(defconstant +mana-regen-rate+ 0.05)

(defun regenerate-mana (dt)
  (incf *mana* (* dt +mana-regen-rate+)))
