(defpackage #:mana-break
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria #:array-length #:define-constant
                #:make-keyword #:non-negative-fixnum #:positive-fixnum #:switch)
  (:import-from #:global-vars #:define-global-parameter)
  (:export #:main))
