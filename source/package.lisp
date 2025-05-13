(cl:defpackage #:pantalea.errors
  (:use #:cl)
  (:export
   #:*chain-enabled*
   #:def
   #:chain-error-cause
   #:!!!
   #:make-chain-condition
   #:with-link))
