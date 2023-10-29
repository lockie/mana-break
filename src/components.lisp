(in-package #:mana-break)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type single-float +tile-size+))
  (defconstant +tile-size+ 32.0))

;; NOTE: negative map coords are not supported
(deftype pos () '(single-float 0f0 #.(/ +tile-size+ single-float-epsilon)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline tile-start)
           (ftype (function (pos pos) (values pos pos)) tile-start))
  (defun tile-start (x y)
    (values
     (* +tile-size+ (the fixnum (floor x +tile-size+)))
     (* +tile-size+ (the fixnum (floor y +tile-size+)))))

  (declaim
   (inline tile-hash)
   (ftype (function (pos pos) fixnum) tile-hash))
  (defun tile-hash (x y)
    (let+ (((&values tile-x tile-y) (tile-start x y))
           (x* (truncate tile-x))
           (y* (truncate tile-y)))
      (declare (type (integer 0 2147483647) x* y*))
      (logior (ash x* 31) y*))))

(declaim (inline marshal-tile)
         (ftype (function (fixnum) (values pos pos)) marshal-tile))
(defun marshal-tile (hash)
  (values
   (float (ash hash -31))
   (float (logand hash 2147483647))))

(ecs:defcomponent position
  "The object position in pixels."
  (x 0.0 :type pos
         :documentation "x position aka screen pixel coordinate")
  (y 0.0 :type pos
         :documentation "y position aka screen pixel coordinate")
  (tile (tile-hash x y)
        :type fixnum :index tiles
        :documentation "Tile index, for fast map tile lookups."))

(ecs:defcomponent size
  "The object size in pixels."
  (width  0.0 :type pos)
  (height 0.0 :type pos))

(ecs:defcomponent image
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))
