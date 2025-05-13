(cl:defpackage #:pantalea.errors
  (:use #:cl)
  (:export
   #:*chain-enabled*
   #:def
   #:!!!
   #:make-chain-condition
   #:with-link))
