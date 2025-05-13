(cl:in-package #:pantalea.errors)


(define-condition link-mixin (cl:error)
  ())

(define-condition chain-error (cl:error)
  ((%cause :initform nil
           :accessor chain-error-cause))
  (:report (lambda (condition stream)
             (format stream "Error ~a was signaled" (type-of condition))
             (alexandria:when-let ((cause (chain-error-cause condition)))
               (format stream " caused by:~%~a" cause)))))

(defgeneric chain-error-root-cause (cl:error)
  (:method ((error cl:error))
    error)
  (:method ((error chain-error))
    (chain-error-root-cause (chain-error-cause error))))

(defun link-error-name (error-name)
  (intern (format nil "~a/LINKED" error-name)))

(defmacro def (error-name (&rest parent-types) (&rest slot-specs) &body options)
  `(progn
     (define-condition ,error-name ,(if (endp parent-types) '(chain-error) parent-types)
       ,slot-specs
       ,@options)
     (define-condition ,(link-error-name error-name) (link-mixin ,error-name)
       ())))

(defparameter *chain-enabled* t)

(defmacro with-link (error-form (&rest enabled-errors) &body body)
  (alexandria:with-gensyms (!e)
    `(handler-case (progn ,@body)
       ((and link-mixin (not (or ,@enabled-errors))) (,!e)
         (let ((result-error ,error-form))
           (setf (chain-error-reason result-error) ,!e)
           (cl:error result-error))))))

(defun make-chain-condition (type &rest args)
  (if *chain-enabled*
      (apply #'make-condition (link-error-name type) args)
      (apply #'make-condition type args)))

(defmacro !!! (type &rest args)
  `(if *chain-enabled*
      (cl:error  ',(link-error-name type) ,@args)
      (cl:error ',type ,@args)))
