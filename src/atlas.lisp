(in-package #:mana-break)


(ecs:defcomponent (frame
                   :finalize #'(lambda (entity &key bitmap &allow-other-keys)
                                 (when (has-frame-prefab-p entity)
                                   (al:destroy-bitmap bitmap))))
  "Single sprite frame."
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer)
  (sequence-number 0 :type fixnum)
  (time 0.0 :type single-float))

(ecs:defcomponent (frame-prefab
                   :composite-index
                   (frame-prefab (name sequence-number) :unique t))
  "Prefab component for a single sprite frame."
  (name "" :type string :index frame-prefabs)
  (sequence-number 0 :type fixnum))

(ecs:defcomponent sprite
  "Specific sprite state."
  (name "" :type string)
  (current-frame 0 :type fixnum)
  (frame-count 1 :type #-ccl positive-fixnum #+ccl (integer #.(expt 2 32)))
  (time 0.0 :type single-float))

(defun load-atlas (path)
  (flet ((split (str sep)
           (let ((s (uiop:split-string str :separator (list sep))))
             (values
              (first s)
              (second s)))))
    (uiop:with-safe-io-syntax ()
      (loop :with lines :=
               (with-open-stream
                   (stream (al:make-character-stream
                            (merge-pathnames #P"../Resources/atlas/" path)))
                 (uiop:slurp-stream-lines stream))
            :with image-filename := (first lines)
            :with bitmap := (al:ensure-loaded #'al:load-bitmap image-filename)
            :with name := image-filename
            :with x :and y :and width :and height :and i
            :for line :in (rest lines)
            :do (if (find #\: line)
                    (let+ ((line* (string-trim '(#\Space) line))
                             ((&values key value) (split line* #\:)))
                        (switch (key :test #'string=)
                          ("xy" (multiple-value-setq (x y)
                                  (split value #\,)))
                          ("size" (multiple-value-setq (width height)
                                    (split value #\,)))
                          ("index" (setf i value))))
                    (progn
                      (when i
                        (let* ((sx (parse-integer x))
                               (sy (parse-integer y))
                               (w (parse-integer width))
                               (h (parse-integer height))
                               (n (parse-integer i))
                               (seq (if (minusp n) 0 n))
                               (sub-bitmap
                                 (al:create-sub-bitmap bitmap sx sy w h)))
                          (ecs:make-object
                           `((:frame-prefab :name ,name
                                            :sequence-number ,seq)
                             (:frame :bitmap ,sub-bitmap
                                     :sequence-number ,seq
                                     :time 0.5)
                             (:size :width ,(float w)
                                    :height ,(float h))))))
                      (setf name line)))))))
