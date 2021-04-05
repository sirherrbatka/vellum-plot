(cl:in-package #:vellum-plot)


(defun read-new-value ()
  (format *query-io* "Enter a new value.")
  (force-output *query-io*)
  (eval (read)))


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
              (setf (slot-value (unbound-slot-instance ,!condition)
                                (cell-error-name ,!condition))
                    new-value)
              (go ,!block)))))))
