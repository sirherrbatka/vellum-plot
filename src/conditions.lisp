(cl:in-package #:vellum-plot)


(define-condition missing-required-slot (cl-ds:initialization-error)
  ((%keyarg :initarg :keyarg)
   (%slot-name :initarg :slot-name
               :reader slot-name)
   (%object :initarg :object
            :reader layer)))


(defun missing-required-slot (slot-name keyarg object)
  (error 'missing-required-slot
         :keyarg keyarg
         :object object
         :slot-name slot-name
         :format-control "No value ~a for initialization of the ~a."
         :format-arguments (list keyarg object)))


(defun read-new-value ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))


(defmacro with-bind-slot-restart (&body body)
  (with-gensyms (!condition !block)
    `(tagbody ,!block
        (let ((,!condition nil))
          (restart-case (handler-case (progn ,@body)
                          (condition (e)
                            (setf ,!condition e)
                            (error e)))
            (bind-slot (new-value)
              :interactive read-new-value
              :report "Bind slot of the object."
              (setf (slot-value (layer ,!condition)
                                (slot-name ,!condition))
                    new-value)
              (go ,!block)))))))
