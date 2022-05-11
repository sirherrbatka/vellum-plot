(cl:in-package #:vellum-plot)


(defmethod cl-ds.utils:cloning-information append
    ((stack stack-of-layers))
  `((:data-layer data-layer)
    (:aesthetics-layer aesthetics-layer)
    (:scale-layer scale-layer)
    (:geometrics-layers geometrics-layers)
    (:statistics-layer statistics-layer)
    (:facets-layer facets-layer)
    (:coordinates-layer coordinates-layer)))


(defmethod add ((data vellum.table:fundamental-table)
                (layer fundamental-layer))
  (declare (optimize (speed 0)))
  (make 'stack-of-layers
        :data-layer data
        (layer-category layer)
        layer))


(defmethod layer-category ((layer geometrics-layer))
  :geometrics-layer)


(defmethod layer-category ((layer aesthetics-layer))
  :aesthetics-layer)


(defmethod layer-category ((layer facets-layer))
  :facets-layer)


(defmethod add ((stack stack-of-layers)
                (layer fundamental-layer))
  (cl-ds.utils:quasi-clone stack
                           (layer-category layer)
                           layer))


(defmethod add ((stack stack-of-layers)
                (layer geometrics-layer))
  (cl-ds.utils:quasi-clone
   stack
   :geometrics-layers (cons layer (geometrics-layers stack))))


(defmethod vellum:visualize ((backend (eql :plotly))
                             (stack stack-of-layers)
                             (destination stream)
                             &rest options
                             &key (only-script nil) (div-id "plotDiv"))
  (declare (ignore options))
  (plotly-visualize stack destination :only-script only-script :div-id div-id))


(defmethod vellum:visualize ((backend (eql :plotly))
                             (stack stack-of-layers)
                             destination
                             &rest options
                             &key (only-script nil) (div-id "plotDiv"))
  (declare (ignore only-script div-id))
  (with-output-to-file (stream destination)
    (apply #'vellum:visualize :plotly stack stream options)))
