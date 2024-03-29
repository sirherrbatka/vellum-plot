(cl:in-package #:vellum-plot)


(defgeneric data-layer (stack))
(defgeneric aesthetics-layer (stack))
(defgeneric scale-layer (stack))
(defgeneric geometrics-layers (stack))
(defgeneric statistics-layer (stack))
(defgeneric facets-layer (stack))
(defgeneric coordinates-layer (stack))
(defgeneric x (layer))
(defgeneric y (layer))
(defgeneric z (layer))
(defgeneric color (layer))
(defgeneric shape (layer))
(defgeneric size (layer))
(defgeneric label (layer))
(defgeneric add (stack layer))
(defgeneric layer-category (layer))
(defgeneric height (layer))
(defgeneric width (layer))
(defgeneric dtick (layer))
(defgeneric tick-length (axis))
(defgeneric range (axis))
(defgeneric constrain (axis))
(defgeneric scale-anchor (axis))
(defgeneric rows (layer))
(defgeneric columns (layer))
(defgeneric label (layer))
(defgeneric mode (layer))
