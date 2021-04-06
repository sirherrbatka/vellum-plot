(cl:in-package #:vellum-plot)


(defun read-new-value ()
  (format *query-io* "Enter a new value.")
  (force-output *query-io*)
  (eval (read)))


(defmacro with-bind-slot-restart (&body body)
  (with-gensyms (!condition !block !result !return)
    `(block ,!return
       (tagbody ,!block
          (let ((,!condition nil)
                (,!result nil))
            (restart-case (handler-case (setf ,!result (progn ,@body))
                            (condition (e)
                              (setf ,!condition e)
                              (error e)))
              (bind-slot (new-value)
                :interactive read-new-value
                :report "Bind slot of the object."
                (setf (slot-value (unbound-slot-instance ,!condition)
                                  (cell-error-name ,!condition))
                      new-value)
                (go ,!block)))
            (return-from ,!return ,!result))))))
