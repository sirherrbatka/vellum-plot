(cl:in-package #:vellum-plot)


(defgeneric data-layer (stack))
(defgeneric aesthetics-layer (stack))
(defgeneric scale-layer (stack))
(defgeneric geometrics-layers (stack))
(defgeneric statistics-layer (stack))
(defgeneric facets-layer (stack))
(defgeneric coordinates-layer (stack))
(defgeneric x (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :x
               :slot-name '%x
               :object layer)))))
(defgeneric y (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :y
               :slot-name '%y
               :object layer)))))
(defgeneric z (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :z
               :slot-name '%z
               :object layer)))))
(defgeneric color (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :color
               :slot-name '%color
               :object layer)))))
(defgeneric shape (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :shape
               :slot-name '%shape
               :object layer)))))
(defgeneric size (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :size
               :slot-name '%size
               :object layer)))))
(defgeneric label (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :label
               :slot-name '%label
               :object layer)))))
(defgeneric add (stack layer))
(defgeneric layer-category (layer))
(defgeneric height (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :height
               :slot-name '%height
               :object layer)))))
(defgeneric width (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :height
               :slot-name '%height
               :object layer)))))
(defgeneric dtick (layer)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :dtick
               :slot-name '%dtick
               :object layer)))))
(defgeneric tick-length (axis)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :tick-length
               :slot-name '%tick-length
               :object layer)))))
(defgeneric range (axis)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :range
               :slot-name '%range
               :object layer)))))
(defgeneric constrain (axis)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :constrain
               :slot-name '%constrain
               :object layer)))))
(defgeneric scale-anchor (axis)
  (:method :around (layer)
    (handler-case (call-next-method)
      (unbound-slot (e) (declare (ignore e))
        (error 'missing-required-slot
               :keyarg :scale-anchor
               :slot-name '%scale-anchor
               :object layer)))))
