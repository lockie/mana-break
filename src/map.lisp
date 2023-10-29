(in-package #:mana-break)


(defconstant +max-movement-cost+ 1000000)

(declaim (type array-length *world-width* *world-height*))
(define-global-parameter *world-width* 0 "World width in tiles")
(define-global-parameter *world-height* 0 "World height in tiles")

(ecs:defcomponent map-tile
  (movement-cost 0 :type fixnum))

(declaim (ftype (function (fixnum) fixnum) total-map-tile-movement-cost))
(defun total-map-tile-movement-cost (tile-hash)
  (let ((sum 0))
    (declare (type fixnum sum))
    (with-tiles tile-hash tile
      (when (has-map-tile-p tile)
        (incf sum (map-tile-movement-cost tile))))
    sum))

(declaim (ftype (function (pos pos) boolean) obstaclep))
(defun obstaclep (x y)
  "Takes tile position and returns T if there are obstacles in there."
  (with-tiles (tile-hash x y) tile
    (when (and (has-map-tile-p tile)
               (<= +max-movement-cost+ (map-tile-movement-cost tile)))
      (return t))))

(ecs:defsystem render-map-tiles
  (:components-ro (position image map-tile)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap image-bitmap position-x position-y 0))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(defun properties->spec (properties)
  (loop :for component :being :the :hash-key
        :using (hash-value slots) :of properties
        :when (typep slots 'hash-table)
        :collect (list* (make-keyword (string-upcase component))
                        (loop :for name :being :the :hash-key
                              :using (hash-value value) :of slots
                              :nconcing (list
                                         (make-keyword (string-upcase name))
                                         value)))))

(defun read-file-into-string (pathname &key (buffer-size 4096))
  (with-open-stream (stream (al:make-character-stream pathname))
    (alexandria:read-stream-content-into-string
     stream :buffer-size buffer-size)))

(defun load-map (filename)
  (let* ((map (tiled:load-map
               filename
               (lambda (path &rest rest)
                 (apply #'read-file-into-string
                        (merge-pathnames #P"../Resources/maps/" path)
                        rest))))
         (tile-width (tiled:map-tile-width map))
         (tile-height (tiled:map-tile-height map))
         (map-width (tiled:map-width map))
         (map-height (tiled:map-height map))
         (tilemap (make-hash-table)))
    (setf *world-width* map-width
          *world-height* map-height)
    (dolist (tileset (tiled:map-tilesets map))
      (let ((bitmap (load-bitmap
                     (merge-pathnames
                      #P"../Resources/images/"
                      (file-namestring
                       (tiled:image-source
                        (tiled:tileset-image tileset)))))))
        (dolist (tile (tiled:tileset-tiles tileset))
          (let* ((external-tile-spec
                   (when (typep tile 'tiled:tiled-tileset-tile)
                     (properties->spec (tiled:properties tile))))
                 (map-tile-component-in-external-spec-p
                   (assoc :map-tile external-tile-spec))
                 (internal-tile-spec
                   `((:image :bitmap ,(al:create-sub-bitmap
                                       bitmap
                                       (tiled:tile-pixel-x tile)
                                       (tiled:tile-pixel-y tile)
                                       tile-width
                                       tile-height))
                     (:size :width ,(float tile-width)
                            :height ,(float tile-height))
                     ,@(unless map-tile-component-in-external-spec-p
                         '((:map-tile)))))
                 (tile-spec (append internal-tile-spec external-tile-spec)))
            (setf (gethash tile tilemap) (ecs:make-object tile-spec))))))
    (dolist (layer (tiled:map-layers map))
      (cond
        ((typep layer 'tiled:tile-layer)
         (dolist (cell (tiled:layer-cells layer))
           (let ((tile-entity (gethash (tiled:cell-tile cell) tilemap))
                 (x (float (tiled:cell-x cell)))
                 (y (float (tiled:cell-y cell)))
                 (tile-instance (ecs:make-entity)))
             (replace-image tile-instance tile-entity)
             (replace-size tile-instance tile-entity)
             (replace-map-tile tile-instance tile-entity)
             (when (has-resource-p tile-entity)
               (replace-resource tile-instance tile-entity))
             (when (has-storage-p tile-entity)
               (delete-storage tile-entity)
               (make-storage tile-instance))
             (make-position tile-instance :x x :y y))))
        ((typep layer 'tiled:object-layer)
         (dolist (object (tiled:object-group-objects layer))
           (let ((entity (ecs:make-object
                          (with-input-from-string
                              (s (gethash "object" (tiled:properties object)))
                            (read s)))))
             (make-position entity
                            :x (float (tiled:object-x object))
                            :y (float (tiled:object-y object))))))))
    (loop :for tile-entity :of-type ecs:entity
          :being :the :hash-values :of tilemap
          :do (ecs:delete-entity tile-entity))))
