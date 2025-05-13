(cl:defpackage #:pantalea.errors
  (:use #:cl)
  (:export
   #:*chain-enabled*
   #:def
   #:chained-error-cause
   #:chained-error-root-cause
   #:!!!
   #:make-chained
   #:with-link))
