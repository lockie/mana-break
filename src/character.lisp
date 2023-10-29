(in-package #:mana-break)


(ecs:defcomponent character
  (speed 0.0 :type single-float))

(declaim (inline distance))
(defun distance (x1 y1 x2 y2)
  (flet ((sqr (x) (* x x)))
    (declare (inline sqr))
    (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))

(ecs:defcomponent target
    (entity -1 :type ecs:entity))

(defconstant +sqrt2+ (sqrt 2))

(declaim (inline heuristic-cost))
(defun heuristic-cost (x1 y1 x2 y2)
  "Octile distance"
  (declare (type single-float x1 y1 x2 y2))
  (let ((dx (abs (the single-float (- x1 x2))))
        (dy (abs (the single-float (- y1 y2)))))
    (declare (type single-float dx dy))
    (+ (* +sqrt2+ (min dx dy))
       (abs (- dx dy)))))

(declaim (type (simple-array single-float) +neighbours-x+))
(define-constant +neighbours-x+ (make-array
                                 8 :element-type 'single-float
                                   :initial-contents
                                 '(32.0 32.0 0.0 -32.0 -32.0 -32.0 0.0 32.0))
  :test #'equalp)

(declaim (type (simple-array single-float) +neighbours-y+))
(define-constant +neighbours-y+ (make-array
                                 8 :element-type 'single-float
                                   :initial-contents
                                 '(0.0 -32.0 -32.0 -32.0 0.0 32.0 32.0 32.0))
  :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&stack (x) :once-only? nil)
    `(let ((,x ,let-plus::value))
       (declare (dynamic-extent ,x))
       ,@let-plus::body))
  (define-let+-expansion (&static (x) :once-only? nil)
    `(let ((,x (load-time-value ,let-plus::value)))
       ,@let-plus::body)))

(declaim (type (simple-array single-float) *cost-so-far*))
(global-vars:define-global-parameter *cost-so-far*
    (make-array 0 :element-type 'single-float))

(declaim (type (simple-array fixnum) *came-from*))
(global-vars:define-global-parameter *came-from*
    (make-array 0 :element-type 'fixnum))

(declaim
 (ftype
  (function
   (single-float single-float single-float single-float) (or fixnum null))
  a*))
(defun a* (start-x start-y goal-x goal-y)
  (let+ (((&values start-x* start-y*) (tile-start start-x start-y))
         ((&values goal-x* goal-y*) (tile-start goal-x goal-y))
         (world-width *world-width*)
         (world-size (the array-length (* world-width *world-height*)))
         ((&macrolet cost-so-far (x y)
            `(aref *cost-so-far*
                   (+ (the array-index (* (floor ,y +tile-size+) world-width))
                      (floor ,x +tile-size+)))))
         ((&stack frontier-items)
          (make-array 512 :element-type 'fixnum))
         ((&stack frontier-priorities)
          (make-array 512 :element-type 'single-float))
         ((&stack frontier) (make-queue frontier-items frontier-priorities)))
    (when (< (length *cost-so-far*) world-size)
      (setf *cost-so-far* (ecs::adjust-array* *cost-so-far* world-size
                                              :element-type 'single-float)))
    (when (< (length *came-from*) world-size)
      (setf *came-from* (ecs::adjust-array* *came-from* world-size
                                            :element-type 'fixnum)))
    (fill *cost-so-far* float-features:single-float-nan)
    (fill *came-from* -1)
    (queue-insert frontier (tile-hash start-x* start-y*) 0.0)
    (setf (cost-so-far start-x* start-y*) 0.0)
    (loop
      :with current :of-type fixnum
      :with current-x single-float :and current-y single-float
      :when (queue-empty-p frontier) :do (return nil)
      :do (setf current (queue-pop frontier))
          (multiple-value-setq (current-x current-y) (marshal-tile current))
      :when (and (= current-x goal-x*) (= current-y goal-y*))
        :do (return current)
      :do (loop
            :for delta-x :of-type single-float :across +neighbours-x+
            :for delta-y :of-type single-float :across +neighbours-y+
            :for next-x :of-type single-float := (+ current-x delta-x)
            :for next-y :of-type single-float := (+ current-y delta-y)
            :when (and (plusp next-x) (plusp next-y)
                       (< next-x (float +window-width+))
                       (< next-y (float +window-height+)))
            :do (let* ((next-hash (tile-hash next-x next-y))
                       (new-cost (+ (cost-so-far current-x current-y)
                                    (total-map-tile-movement-cost next-hash)
                                    (heuristic-cost delta-x delta-y 0.0 0.0)))
                       (next-cost (cost-so-far next-x next-y)))
                  (when (< new-cost +max-movement-cost+)
                    (incf new-cost (float (if (and (not (zerop delta-x))
                                                   (not (zerop delta-y)))
                                              ;; diagonal movement
                                              (+ (total-map-tile-movement-cost
                                                  (tile-hash current-x next-y))
                                                 (total-map-tile-movement-cost
                                                  (tile-hash next-x current-y)))
                                              0)))
                    (when (and (< new-cost +max-movement-cost+)
                               (or (float-features:float-nan-p next-cost)
                                   (< new-cost next-cost)))
                      (setf (cost-so-far next-x next-y) new-cost
                            (aref *came-from*
                                  (+ (the array-index
                                          (* (floor next-y +tile-size+)
                                             world-width))
                                     (floor next-x +tile-size+)))
                            (tile-hash current-x current-y))
                      (queue-insert frontier next-hash
                                    (+ new-cost (heuristic-cost
                                                 next-x next-y
                                                 goal-x* goal-y*))))))))))

(ecs:defcomponent follows-path)

(ecs:defcomponent path-point
  (x 0.0 :type pos)
  (y 0.0 :type pos)
  (traveller -1 :type ecs:entity :index path-points))

(declaim (ftype (function (fixnum fixnum ecs:entity)) reconstruct-path))
(defun reconstruct-path (start goal entity)
  (let* ((size (length *came-from*))
         (length 0)
         (path (make-array size :element-type 'fixnum)))
    (declare (dynamic-extent path)
             (type array-length size length))
    (setf (aref path length) goal)
    (loop :for c :of-type fixnum := goal
            :then (multiple-value-bind (x y)
                      (marshal-tile c)
                    (aref *came-from*
                          (+ (the array-index (* (floor y +tile-size+)
                                                 *world-width*))
                             (floor x +tile-size+))))
          :until (minusp c) :do
            (setf (aref path (incf length)) c))
    (setf (aref path (incf length)) start)
    (loop :for i :from length :downto 0
          :do (multiple-value-bind (x y)
                  (marshal-tile (aref path i))
                (ecs:make-object
                 `((:path-point :x ,x :y ,y :traveller ,entity)))))))

(define-behaviour-tree-node calculate-path
    ((target-tile-x -1.0 :type single-float)
     (target-tile-y -1.0 :type single-float))
    (:components-ro (position))
  "Calculates the path points using A* algorithm."
  (cond
    ((not (has-target-p entity))
     (complete-node nil))
    ((and (not (minusp calculate-path-target-tile-x))
          (with-position (x y) (target-entity entity)
            (multiple-value-bind (tile-x tile-y)
                (tile-start x y)
              (and (= calculate-path-target-tile-x tile-x)
                   (= calculate-path-target-tile-y tile-y)))))
     (complete-node t))
    (t
     (with-position (goal-x goal-y)
         (target-entity entity)
       (if-let (goal (a* position-x position-y goal-x goal-y))
         (multiple-value-bind (tile-x tile-y)
             (tile-start goal-x goal-y)
           (setf calculate-path-target-tile-x tile-x
                 calculate-path-target-tile-y tile-y)
           (assign-follows-path entity)
           (reconstruct-path (tile-hash position-x position-y) goal entity)
           (complete-node t))
         (complete-node nil))))))

(ecs:defcomponent movement
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float))

(declaim (inline approx-equal))
(defun approx-equal (a b &optional (epsilon 0.01))
  (< (abs (- a b)) epsilon))

(define-behaviour-tree-node follow-path ()
    (:components-rw (position))
  (let ((path-points (path-points entity :count 2)))
    (if-let (first-point (first path-points))
      (with-path-point (path-point-x path-point-y) first-point
        (if (approx-equal
             0
             (distance position-x position-y path-point-x path-point-y))
            (block point-reached
              (setf position-x path-point-x
                    position-y path-point-y)
              (ecs:delete-entity first-point)
              (if-let (next-point (second path-points))
                (with-path-point (next-point-x next-point-y) next-point
                  (assign-movement entity :target-x next-point-x
                                          :target-y next-point-y)
                  (complete-node t))
                (block path-completed
                  (delete-follows-path entity)
                  (complete-node nil))))
            (unless (has-movement-p entity)
              (delete-follows-path entity)
              (dolist (point (path-points entity))
                (ecs:delete-entity point)))))
      (complete-node nil))))

(defconstant +epsilon+ 0.1)

(defun character-obstructed (x y)
  (or
   (obstaclep x y)
   (obstaclep (+ x +tile-size+ (- +epsilon+)) y)
   (obstaclep x (+ y +tile-size+ (- +epsilon+)))
   (obstaclep (+ x +tile-size+ (- +epsilon+)) (+ y +tile-size+ (- +epsilon+)))))

(define-behaviour-tree-node move ()
    (:components-ro (character movement)
     :components-rw (position)
     :arguments ((:dt single-float)))
  (if (approx-equal
       0
       (distance position-x position-y movement-target-x movement-target-y))
      (block finished
        (setf position-x movement-target-x
              position-y movement-target-y)
        (delete-movement entity)
        (complete-node t))
      (let* ((diff-x (- movement-target-x position-x))
             (diff-y (- movement-target-y position-y))
             (max-delta (sqrt (+ (* diff-x diff-x) (* diff-y diff-y))))
             (delta (min (* dt character-speed) max-delta))
             (angle (atan diff-y diff-x))
             (delta-x (* delta (cos angle)))
             (delta-y (* delta (sin angle)))
             (new-x (+ position-x delta-x))
             (new-y (+ position-y delta-y)))
        (declare
         (type single-float
               diff-x diff-y delta angle delta-x delta-y new-x new-y))
        (if (character-obstructed new-x new-y)
            (block stuck
              (format t "character ~a stuck @ ~a ~a~%" entity new-x new-y)
              (delete-movement entity)
              (complete-node nil))
            (setf position-x new-x
                  position-y new-y)))))

(define-behaviour-tree-node make-random-movement-target () nil
  (loop :for x :of-type single-float :=
           (random (* +tile-size+ *world-width*))
        :for y :of-type single-float :=
           (random (* +tile-size+ (1- *world-height*)))
        :while (obstaclep x y)
        :finally (with-tiles (tile-hash x y) tile
                   (when (has-map-tile-p tile)
                     (assign-target entity :entity tile)
                     (return-from ecs::current-entity (complete-node t))))))

(define-behaviour-tree idle
    (repeat (:name "loop")
            (sequence (:name "root")
                      (make-random-movement-target ())
                      (calculate-path ())
                      (repeat-until-fail
                       (:name "follow-path-loop")
                       (sequence (:name "follow-sequence")
                                 (follow-path ())
                                 (move ())))
                      (idle (:time 5.0)))))

(defconstant +tree+ 0)
(defconstant +ore+ 1)

(ecs:defcomponent resource
  "Either wood or ore deposit."
  (type -1 :type fixnum :index resource-of-type)
  (free 1 :type bit))

(define-behaviour-tree-node pick-tree () nil
  (with-resource-of-type +tree+ resource
    (when (plusp (resource-free resource))
      (assign-target entity :entity resource)
      (return-from ecs::current-entity (complete-node t))))
  (complete-node nil))

(defconstant +gather-distance+ 32.0)

(define-behaviour-tree-node start-collecting ()
    (:components-ro (position target))
  (with-position (target-x target-y) target-entity
    (with-resource () target-entity
      (when (or (zerop free)
                (< +gather-distance+
                   (distance position-x position-y
                             target-x target-y)))
        (return-from ecs::current-entity (complete-node nil)))
      (setf free 0)
      (complete-node t))))

(define-behaviour-tree-node finish-collecting ()
    (:components-ro (target))
  (with-resource () target-entity
    (setf free 1)
    (complete-node t)))

(ecs:defcomponent storage
  (storage 1 :type bit :index storage-entity :unique t))

(define-behaviour-tree-node start-carrying () nil
  (assign-target entity :entity (storage-entity 1))
  (complete-node t))

(define-behaviour-tree-node store-wood () nil
  ;; TODO degrade efficiency related to the number of workshops?..
  (incf *wood* 5.0)
  (complete-node t))

(define-behaviour-tree-node flip-coin
    ((probability 0.5 :type single-float))
    nil
  (complete-node (> flip-coin-probability (random 1.0))))

(define-behaviour-tree-node start-idling () nil
  (delete-tree)
  (make-idle-behaviour-tree entity))

(define-behaviour-tree collect-wood
    (sequence
     (:name "root")
     (repeat-until-fail
      (:name "collecting-wood-loop")
      (sequence (:name "collect-wood-sequence")
                (pick-tree ())
                (calculate-path (:name "calculate-path-to-tree"))
                (repeat-until-fail
                 (:name "following-tree-loop")
                 (sequence (:name "follow-tree-sequence")
                           (follow-path (:name "keep-following-tree"))
                           (move (:name "follow-tree"))))
                (start-collecting ())
                (idle (:name "collect-wood" :time 2.0))
                (finish-collecting ())
                (start-carrying ())
                (calculate-path (:name "calculate-path-to-storage"))
                (repeat-until-fail
                 (:name "carrying-loop")
                 (sequence (:name "carry-sequence")
                           (follow-path (:name "keep-carrying"))
                           (move (:name "carry"))))
                (idle (:name "unload" :time 1.0))
                (store-wood ())
                (flip-coin (:name "concentration" :probability 0.8))))
     (start-idling ())))

(defconstant +workshop+ 0)
(defconstant +temple+ 1)

(ecs:defcomponent building
  (assigned 0 :type bit :index assigned-buildings)
  (type -1 :type fixnum :index buildings-of-type))

(define-behaviour-tree-node check-workshop-present () nil
  (with-buildings-of-type +workshop+ _
    (return-from ecs::current-entity (complete-node t)))
  (complete-node nil))

(define-behaviour-tree-node pick-ore () nil
  (with-resource-of-type +ore+ resource
    (when (plusp (resource-free resource))
      (assign-target entity :entity resource)
      (return-from ecs::current-entity (complete-node t))))
  (complete-node nil))

(define-behaviour-tree-node store-ore () nil
  ;; TODO degrade efficiency related to the number of workshops?..
  (incf *ore* 5.0)
  (complete-node t))

(define-behaviour-tree collect-ore
    (sequence
     (:name "root")
     (repeat-until-fail
      (:name "collecting-ore-loop")
      (sequence (:name "collect-ore-sequence")
                (check-workshop-present ())
                (pick-ore ())
                (calculate-path (:name "calculate-path-to-ore"))
                (repeat-until-fail
                 (:name "following-ore-loop")
                 (sequence (:name "follow-ore-sequence")
                           (follow-path (:name "keep-following-ore"))
                           (move (:name "follow-ore"))))
                (start-collecting ())
                (idle (:name "collect-ore" :time 2.0))
                (finish-collecting ())
                (start-carrying ())
                (calculate-path (:name "calculate-path-to-storage"))
                (repeat-until-fail
                 (:name "carrying-loop")
                 (sequence (:name "carry-sequence")
                           (follow-path (:name "keep-carrying"))
                           (move (:name "carry"))))
                (idle (:name "unload" :time 1.0))
                (store-ore ())
                (flip-coin (:name "concentration" :probability 0.6))))
     (start-idling ())))

(define-behaviour-tree-node check-resources
    ((wood 0.0 :type single-float)
     (ore 0.0 :type single-float))
    nil
  (complete-node
   (and (>= *wood* check-resources-wood)
        (>= *ore* check-resources-ore))))

(define-behaviour-tree-node pick-building () nil
  (with-assigned-buildings 0 building
    (assign-target entity :entity building)
    (return-from ecs::current-entity (complete-node t)))
  (complete-node nil))

(defconstant +build-distance+ 32.0)

(define-behaviour-tree-node start-building ()
    (:components-ro (position target))
  (with-position (target-x target-y) target-entity
    (with-building () target-entity
      (when (or (plusp assigned)
                (< +build-distance+
                   (distance position-x position-y target-x target-y)))
        (return-from ecs::current-entity (complete-node nil)))
      (setf assigned 1)
      (complete-node t))))

(define-behaviour-tree-node finish-building-workshop ()
    (:components-ro (target))
  (with-building () target-entity
    (setf type +workshop+))
  (with-position (target-x target-y) target-entity
    (make-sprite-entity "smithy sign" target-x (- target-y +tile-size+)))
  (complete-node t))

(define-behaviour-tree-node spend-resources
    ((wood 0.0 :type single-float)
     (ore 0.0 :type single-float))
    nil
  (if (and (>= *wood* spend-resources-wood)
           (>= *ore* spend-resources-ore))
      (progn
        (decf *wood* spend-resources-wood)
        (decf *ore* spend-resources-ore)
        (complete-node t))
      (complete-node nil)))

(define-behaviour-tree build-workshop
    (sequence
     (:name "root")
     (repeat-until-fail
      ()
      (sequence
       (:name "build-workshop-sequence")
       (check-resources (:wood 100.0))
       (pick-building ())
       (calculate-path (:name "calculate-path-to-building"))
       (repeat-until-fail
        (:name "following-building")
        (sequence (:name "follow-building-sequence")
                  (follow-path (:name "keep-following-building"))
                  (move ())))
       (flip-coin (:name "concentration" :probability 0.9))
       (spend-resources (:wood 100.0))
       (start-building ())
       (idle (:name "build" :time 10.0))
       (finish-building-workshop ())
       (dummy-false ())))
     (start-idling ())))

(define-behaviour-tree-node finish-building-temple ()
    (:components-ro (target))
  (with-building () target-entity
    (setf type +temple+))
  (with-position (target-x target-y) target-entity
    (make-sprite-entity "church sign" target-x (- target-y +tile-size+)))
  (complete-node t))

(define-behaviour-tree build-temple
    (sequence
     (:name "root")
     (repeat-until-fail
      ()
      (sequence
       (:name "build-temple-sequence")
       (check-resources (:wood 100.0 :ore 100.0))
       (pick-building ())
       (calculate-path (:name "calculate-path-to-building"))
       (repeat-until-fail
        (:name "following-building")
        (sequence (:name "follow-building-sequence")
                  (follow-path (:name "keep-following-building"))
                  (move ())))
       (flip-coin (:name "concentration" :probability 0.9))
       (spend-resources (:wood 100.0 :ore 100.0))
       (start-building ())
       (idle (:name "build" :time 10.0))
       (finish-building-temple ())
       (dummy-false ())))
     (start-idling ())))

(define-behaviour-tree-node pick-temple () nil
  (with-buildings-of-type +temple+ building
    (assign-target entity :entity building)
    (return-from ecs::current-entity (complete-node t)))
  (complete-node nil))

(define-behaviour-tree-node generate-mana
    ((amount 2.0 :type single-float))
    nil
  (incf *mana* generate-mana-amount)
  (complete-node t))

(define-behaviour-tree pray
    (sequence
     (:name "root")
     (repeat-until-fail
      ()
      (sequence
       (:name "pray-sequence")
       (pick-temple ())
       (calculate-path (:name "calculate-path-to-temple"))
       (repeat-until-fail
        (:name "following-temple")
        (sequence (:name "follow-temple-sequence")
                  (follow-path (:name "keep-following-temple"))
                  (move ())))
       (flip-coin (:name "concentration" :probability 0.9))
       (idle (:name "pray" :time 10.0))
       (generate-mana ())
       (dummy-false ())))
     (start-idling ())))
