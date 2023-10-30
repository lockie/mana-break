(in-package #:mana-break)

(define-constant +window-width+ 1600)
(define-constant +window-height+ 900)

(define-constant +repl-update-interval+ 0.3d0)

(define-constant +font-path+ "../Resources/inconsolata.ttf"
  :test #'string=)
(define-constant +font-size+ 24)

(define-constant +config-path+ "../config.cfg"
  :test #'string=)

(defun init ()
  (ecs:bind-storage)
  (load-atlas "dawnlike.atlas")
  (load-map "map.tmx")
  (make-colonists))

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(declaim (type cffi:foreign-pointer *font* *ui-font* *ui-context*))
(defvar *font*)
(defvar *ui-font*)
(defvar *ui-context*)
(defvar *win* nil)

(defun update (dt)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (regenerate-mana (float dt 0.0))
  (when (and (not *win*) (>= *mana* 100.0))
    (setf *win* t)
    (al:show-native-message-box
     (cffi:null-pointer) "You win!" ""
     "You've beat the game by collecting a 100 points of mana required to escape your durance!"
     (cffi:null-pointer) 0))
  (update-selection)
  (ecs:run-systems :dt (float dt 0.0))
  (nk:with-rects ((window-rect (:x 600 :y 0 :w 400 :h 30)))
    (nk:with-colors ((background (:r 0 :g 0 :b 0 :a 190))
                     (wood (:r 244 :g 164 :b 96))
                     (ore (:r 192 :g 192 :b 192))
                     (mana (:r 65 :g 105 :b 225)))
      (nk:with-styles *ui-context*
          ((:item nk:+style-window-fixed-background+
                  (nk:style-item-color background)))
        (nk:with-window *ui-context* "resources" window-rect
            (:+window-no-scrollbar+ :+window-no-input+) nil
          (nk:layout-row-static *ui-context* 28.0 128 3)
          (nk:label-colored *ui-context*
                            (format nil "wood ~a" (truncate *wood*))
                            (nk:flags nk:text-alignment :+text-left+)
                            wood)
          (nk:label-colored *ui-context*
                            (format nil "mana ~a" (truncate *mana*))
                            (nk:flags nk:text-alignment :+text-centered+)
                            mana)
          (nk:label-colored *ui-context*
                            (format nil "ore ~a" (truncate *ore*))
                            (nk:flags nk:text-alignment :+text-right+)
                            ore))))))

(defun render ()
  #+nil (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                      (format nil "~d FPS" *fps*))
  (nk:allegro-render))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                         (al:show-native-message-box
                          (cffi:null-pointer) "Hey guys"
                          "We got a big error here :("
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace e :stream s))
                          (cffi:null-pointer) :error)))))
    (al:set-app-name "mana-break")
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Mana Break")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *font* := (al:ensure-loaded #'al:load-ttf-font
                                                 +font-path+
                                                 (- +font-size+) 0)
               :with *ui-font* :=  (al:ensure-loaded
                                    #'nk:allegro-font-create-from-file
                                    +font-path+ (- +font-size+) 0)
               :with *ui-context* :=
                  (nk:allegro-init *ui-font* display
                                   +window-width+ +window-height+)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :initially (nk:input-begin *ui-context*)
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :do (nk:allegro-handle-event event)
                        :always (not (eq type :display-close))
                        :finally (nk:input-end *ui-context*))
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (livesupport:continuable
                     (update dt)
                     (render))
                   (al:flip-display)
               :finally
                  (nk:allegro-shutdown)
                  (nk:allegro-font-del *ui-font*)
                  (al:destroy-font *font*)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:uninstall-audio)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %main))))

