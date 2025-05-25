(cl:in-package #:pantalea.errors)


(define-condition link-mixin (cl:error)
  ())

(defparameter *cause* nil)

(defparameter *label* 0)

(defparameter *style* :plain)

(defgeneric chained-error-cause (error))

(defgeneric chained-error-text (error))
(defgeneric chained-error-arguments (error))

(defun json-escape-string (string out)
  "Escape STRING for safe inclusion in JSON."
  (loop for ch across string do
    (case ch
      (#\" (write-string "\\\"" out))
      (#\\ (write-string "\\\\" out))
      (#\Newline (write-string "\\n" out))
      (#\Return (write-string "\\r" out))
      (#\Tab (write-string "\\t" out))
      (#\Backspace (write-string "\\b" out))
      (#\Page (write-string "\\f" out))
      (t (if (or (char<= #\Space ch #\~))
             (write-char ch out)
             ;; Escape non-ASCII control characters as \uXXXX
             (format out "\\u~4,'0X" (char-code ch)))))))

(defun format-cause (error stream)
  (alexandria:when-let ((cause (chained-error-cause error)))
    (alexandria:eswitch (*style*)
      (:plain
       (format stream "~%")
       (format stream "(~a) Caused by:~%~a" *label* cause))
      (:json
       (if (typep cause 'chained-error)
           (format stream ", \"cause\": ~a" cause)
           (let ((*style* :plain))
             (format stream ", \"cause\": \"" )
             (json-escape-string (format nil "~a" cause) stream)
             (format stream "\"")))))))

(defun report-chained-error (condition stream)
  (let ((*label* (1+ *label*)))
    (alexandria:eswitch (*style*)
      (:plain
       (format stream "Error ~a." (type-of condition))
       (alexandria:when-let ((text (chained-error-text condition)))
         (format stream " ")
         (apply #'format text (chained-error-arguments condition)))
       (format-cause condition stream))
      (:json
       (format stream "{\"error_type\": \"" )
       (json-escape-string (symbol-name (type-of condition)) stream)
       (format stream "\"")
       (alexandria:when-let ((text (chained-error-text condition)))
         (format stream ", \"text\": \"")
         (json-escape-string (apply #'format stream text (chained-error-arguments condition))
                             stream)
         (format stream "\""))
       (format-cause condition stream)
       (format stream "}")))))

(define-condition chained-error (cl:error)
  ((%cause :initarg :cause
           :reader chained-error-cause)
   (%text :initarg :text
          :reader chained-error-text)
   (%arguments :initarg :arguments
               :reader chained-error-arguments))
  (:report report-chained-error)
  (:default-initargs :cause *cause*
                     :arguments nil
                     :text nil))

(defgeneric chained-error-root-cause (cl:error)
  (:method ((error cl:error))
    error)
  (:method ((error chained-error))
    (alexandria:if-let ((cause (chained-error-cause error)))
      (chained-error-root-cause cause)
      error)))

(defun link-error-name (error-name)
  (intern (format nil "~a/LINKED" error-name) (symbol-package error-name)))

(defmacro def (error-name (&rest parent-types) (&rest slot-specs) &optional options)
  `(progn
     (define-condition ,error-name ,(if (endp parent-types) '(chained-error) parent-types)
       ,slot-specs
       (,@options
        :report report-chained-error))
     (define-condition ,(link-error-name error-name) (link-mixin ,error-name)
       ()
       (,@options :report report-chained-error))))

(defparameter *chain-enabled* t)

(defmacro with-link (error-form (&rest enabled-errors) &body body)
  (alexandria:with-gensyms (!impl)
    `(flet ((,!impl () ,@body))
       (if *chain-enabled*
           (handler-case (,!impl)
             ((and (not (or ,@enabled-errors)) (or link-mixin (not chained-error)))
               (*cause*)
               (let ((er ,error-form))
                 (error er))))
           (,!impl)))))

(defun make-chained (type &rest args)
  (if *chain-enabled*
      (apply #'make-condition (link-error-name type) args)
      (apply #'make-condition type args)))

(defmacro !!! (type text-args &rest args)
  `(if *chain-enabled*
      (cl:error  ',(link-error-name type)
                 :text ,(first text-args)
                 :arguments (list ,@(rest text-args))
                 ,@args)
      (cl:error ',type
                 :text ,(first text-args)
                 :arguments (list ,@(rest text-args))
                ,@args)))
