(defpackage #:mana-break
  (:use #:cl #:let-plus)
  (:local-nicknames (#:tiled #:cl-tiled))
  (:import-from #:alexandria #:array-index #:array-length #:define-constant
                #:if-let #:make-keyword #:non-negative-fixnum #:positive-fixnum
                #:random-elt #:symbolicate #:switch #:when-let)
  (:import-from #:global-vars #:define-global-parameter)
  (:export #:main))
