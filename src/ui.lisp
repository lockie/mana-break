(in-package #:mana-break)

(ecs:defcomponent selected
  (selected 1 :type bit :index selected-character))

(defun update-selection ()
  (al:with-current-mouse-state state
    (let* ((mouse-state (cffi:mem-ref state '(:struct al:mouse-state)))
           (x (getf mouse-state 'al:x))
           (y (getf mouse-state 'al:y))
           (buttons (getf mouse-state 'al:buttons)))
      (declare (type fixnum x y buttons))
      (let ((selected (first (selected-character 1 :count 1))))
        (when (and (= buttons 1)
                   ;; HACK: dont react to presses within status window
                   (not (and selected
                             (and (> x 1216) (< x 1600)
                                  (> y 230) (< y 670)))))
          (when selected
            (delete-selected selected))
          (with-tiles (tile-hash (float x) (float y)) entity
            (when (has-character-p entity)
              (make-selected entity)
              (return-from update-selection))))))))

(defmacro defspell (caption requirements price behaviour-tree)
  `(progn
     (nk:layout-row-dynamic *ui-context* 56.0 2)
     (,(if (< (length requirements) 8) 'nk:label 'nk:label-wrap)
      *ui-context* ,(concatenate 'string requirements ":")
      ,@(when (< (length requirements) 8)
            '((nk:flags nk:text-alignment :+text-left+))))
     (nk:with-button-label *ui-context* ,caption
       (when (>= *mana* ,price)
         (decf *mana* ,price)
         (stop-activity entity)
         (,(symbolicate :make- behaviour-tree :-behaviour-tree) entity)))))

(define-constant +selection-color+ (al:map-rgb 0 255 0) :test #'equalp)

(ecs:defsystem draw-selection-frame
  (:components-ro (position character selected))
  (al:draw-rectangle position-x position-y
                     (+ position-x +tile-size+) (+ position-y +tile-size+)
                     +selection-color+ 1)
  (nk:with-rects ((window-rect (:x 1216 :y 230 :w 384 :h 440)))
    (nk:with-colors ((background (:r 0 :g 0 :b 0 :a 190))
                     (button-background (:r 65 :g 105 :b 225 :a 140))
                     (button-hover-background (:r 65 :g 105 :b 225 :a 190))
                     (button-active-background (:r 65 :g 105 :b 225 :a 255)))
      (nk:with-styles *ui-context*
          ((:item nk:+style-window-fixed-background+
                  (nk:style-item-color background))
           (:item nk:+style-button-normal+
                  (nk:style-item-color button-background))
           (:item nk:+style-button-hover+
                  (nk:style-item-color button-hover-background))
           (:item nk:+style-button-active+
                  (nk:style-item-color button-active-background)))
        (nk:with-window *ui-context* "status" window-rect
            (:+window-no-scrollbar+) nil
          (nk:layout-row-dynamic *ui-context* 28.0 1)
          (nk:label *ui-context* "COLONIST STATUS"
                    (nk:flags nk:text-alignment :+text-centered+))
          (nk:layout-row-dynamic *ui-context* 28.0 2)
          (nk:label *ui-context* "Nickname:"
                    (nk:flags nk:text-alignment :+text-left+))
          (nk:label *ui-context* character-nickname
                    (nk:flags nk:text-alignment :+text-left+))

          (nk:layout-row-dynamic *ui-context* 56.0 2)
          (nk:label *ui-context* "Activity:"
                    (nk:flags nk:text-alignment :+text-left+))
          (nk:label *ui-context*
                    (cond ((has-idle-root-p entity)
                           "slacking off")
                          ((has-collect-wood-root-p entity)
                           "collecting wood")
                          ((has-collect-ore-root-p entity)
                           "collecting ore")
                          ((has-build-workshop-root-p entity)
                           "building workshop")
                          ((has-build-temple-root-p entity)
                           "building temple")
                          ((has-pray-root-p entity)
                           "praying")
                          (t "unknown"))
                    (nk:flags nk:text-alignment :+text-left+))

          (defspell "collect wood" "1 mana"
            1.0 collect-wood)
          (defspell "collect ore" "2 mana, needs workshop"
            2.0 collect-ore)
          (defspell "build workshop" "4 mana, needs 100 wood"
            4.0 build-workshop)
          (defspell "build temple" "5 mana, needs 50 wood & ore"
            5.0 build-temple)
          (defspell "pray" "3 mana, needs temple"
            3.0 pray))))))
