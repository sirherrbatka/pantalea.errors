(cl:in-package #:pantalea.errors)


(define-condition link-mixin (cl:error)
  ())

(defparameter *cause* nil)

(defgeneric chained-error-cause (error))

(defun format-cause (error stream)
  (alexandria:when-let ((cause (chained-error-cause error)))
    (format stream " caused by:~%~a" cause)))

(define-condition chained-error (cl:error)
  ((%cause :initform *cause*
           :reader chained-error-cause))
  (:report (lambda (condition stream)
             (format stream "Error ~a was signaled" (type-of condition))
             (format-cause condition stream))))

(defgeneric chained-error-root-cause (cl:error)
  (:method ((error cl:error))
    error)
  (:method ((error chained-error))
    (alexandria:if-let ((cause (chained-error-cause error)))
      (chained-error-root-cause cause)
      error)))

(defun link-error-name (error-name)
  (intern (format nil "~a/LINKED" error-name) (symbol-package error-name)))

(defmacro def (error-name (&rest parent-types) (&rest slot-specs) &body options)
  `(progn
     (define-condition ,error-name ,(if (endp parent-types) '(chained-error) parent-types)
       ,slot-specs
       ,@options)
     (define-condition ,(link-error-name error-name) (link-mixin ,error-name)
       ())))

(defparameter *chain-enabled* t)

(defmacro with-link (error-form (&rest enabled-errors) &body body)
  `(handler-case (progn ,@body)
     ((and link-mixin (not (or ,@enabled-errors))) (*cause*)
       ,error-form)))

(defun make-chained (type &rest args)
  (if *chained-enabled*
      (apply #'make-condition (link-error-name type) args)
      (apply #'make-condition type args)))

(defmacro !!! (type &rest args)
  `(if *chained-enabled*
      (cl:error  ',(link-error-name type) ,@args)
      (cl:error ',type ,@args)))
