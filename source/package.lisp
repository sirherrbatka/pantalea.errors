(cl:defpackage #:pantalea.errors
  (:use #:cl)
  (:export
   #:*chain-enabled*
   #:def
   #:chained-error-cause
   #:chained-error-root-cause
   #:format-cause
   #:!!!
   #:make-chained
   #:with-link))
