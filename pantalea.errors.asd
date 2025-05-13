(asdf:defsystem #:pantalea.errors
  :name "errors"
  :depends-on (#:alexandria)
  :serial T
  :pathname "source"
  :components ((:file "package")
               (:file "code")))
