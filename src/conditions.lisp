(cl:in-package #:vellum-plot)


(define-condition missing-required-slot (cl-ds:initialization-error)
  ((%keyarg :initarg :keyarg)
   (%slot-name :initarg :slot-name)
   (%object :initarg :object)))


(defun missing-required-slot (slot-name keyarg object)
  (error 'missing-required-slot
         :keyarg keyarg
         :object object
         :slot-name slot-name
         :format-control "No value ~a for initialization of the ~a."
         :format-arguments (list keyarg object)))
