(cl:in-package #:vellum-plot)


(defvar *json-stream*)
(defvar *depth* 0)


(defmacro slot (name content)
  (with-gensyms (!output)
    `(let ((*depth* (1+ *depth*))
           (,!output (with-output-to-string (*json-stream*)
                       ,content)))
       (unless (emptyp ,!output)
         (format *json-stream* "~a: ~a" ,name ,!output)
         (format *json-stream* ",~%")))))


(defmacro value (content)
  `(let ((result (handler-case ,content
                   (unbound-slot (e)
                     (declare (ignore e)) nil))))
     (unless (null result)
       (json-format *json-stream* result))))


(defmacro valuel (content label)
  `(let ((field-value (handler-case ,content
                   (unbound-slot (e)
                     (declare (ignore e)) nil))))
     (unless (null field-value)
       (format *json-stream* "~a"
               (etypecase field-value
                 (symbol (format nil "'~(~a~)~a'" (symbol-name field-value) ,label))
                 (list (format nil "[~{~a~^, ~}]" field-value))
                 (float (format nil "~F" field-value))
                 (string (format nil "'~a~a'" field-value ,label))
                 (integer field-value))))))


(defun var (content)
  (format *json-stream* "~a" content))


(defmacro object (&body content)
  `(progn
     (format *json-stream* "{")
     ,@content
     (format *json-stream* "}")))


(defmacro json ((stream) &body body)
  `(let ((*json-stream* ,stream))
     ,@body))


(defun plotly-extract-data (table column)
  (if (null column)
      nil
      (vellum.table:with-table (table)
        (cl-ds.alg:to-list
         table
         :key (lambda (&rest ignored)
                (declare (ignore ignored))
                (let ((content (vellum.header:rr column)))
                  (typecase content
                    (double-float (coerce content 'single-float))
                    (string (format nil "'~a'" content))
                    (t content))))))))


(defgeneric plotly-mode (geometrics mapping)
  (:method ((geometrics points-geometrics) mapping)
    (if (label mapping) "markers+text" "markers"))
  (:method ((geometrics line-geometrics) mapping)
    (if (label mapping) "markers+lines" "lines"))
  (:method ((geometrics heatmap-geometrics) mapping)
    nil))


(defgeneric plotly-type (geometrics)
  (:method ((geometrics points-geometrics))
    "scatter")
  (:method ((geometrics heatmap-geometrics))
    "heatmap")
  (:method ((geometrics line-geometrics))
    "scatter"))


(defun json-format (stream field-value)
  (format stream "~a"
          (etypecase field-value
            (symbol (format nil "'~(~a~)'" (symbol-name field-value)))
            (list (format nil "[~{~a~^, ~}]" field-value))
            (float (format nil "~F" field-value))
            (string (format nil "'~a'" field-value))
            (integer field-value))))


(defun plotly-format (stream field-name field-value)
  (format stream "'~a': ~a, ~%" field-name
          (etypecase field-value
            (symbol (format nil "'~(~a~)'" (symbol-name field-value)))
            (list (format nil "[~{~a~^, ~}]" field-value))
            (float (format nil "~F" field-value))
            (string (format nil "'~a'" field-value))
            (integer field-value))))


(defun plotly-format-no-nulls (field-name field-value)
  (if (null field-value)
      nil
      (progn
        (plotly-format *json-stream* field-name field-value)
        t)))

(defun plotly-generate-data/impl (stack geometrics index.table number)
  (bind ((mapping (read-mapping geometrics))
         (facets (facets-layer stack))
         (data (data-layer stack))
         (table (cdr index.table))
         (aesthetics (read-aesthetics geometrics))
         (x (with-bind-slot-restart (x mapping)))
         (y (with-bind-slot-restart (y mapping)))
         (z (with-bind-slot-restart (z mapping)))
         (color (with-bind-slot-restart (color mapping)))
         (shape (with-bind-slot-restart (shape mapping)))
         (label  (with-bind-slot-restart (label mapping)))
         (size (with-bind-slot-restart (size mapping))))
    (declare (ignore shape))
    (macrolet ((set-name (axis)
                 (with-gensyms (!number !data !index)
                     `(unless (null ,axis)
                        (let ((,!number
                                (if (integerp ,axis)
                                    ,axis
                                    (~> data
                                        vellum.table:header
                                        (vellum.header:name-to-index ,axis)))))
                          (if-let ((table-content (gethash ,!number table)))
                            (setf ,axis (car table-content))
                            (let ((,!data (plotly-extract-data data ,axis))
                                  (,!index (incf (car index.table))))
                              (unless (null ,!data)
                                (setf ,axis (format nil "v~a" ,!index)
                                      (gethash ,!number table)
                                      (cons ,axis (with-output-to-string (*json-stream*)
                                                    (value ,!data))))))))))))
      (set-name x)
      (set-name y)
      (set-name z)
      (set-name size)
      (set-name label)
      (set-name color))
    (values
     (with-output-to-string (stream)
       (json (stream)
         (object
           (slot "xaxis" (if (null facets)
                             (value "x")
                             (valuel :x number)))
           (slot "yaxis" (if (null facets)
                             (value "y")
                             (valuel :y number)))
           (slot "x" (var x))
           (slot "y" (var y))
           (unless (null z)
             (slot "z" (var z)))
           (slot "mode" (value (plotly-mode geometrics mapping)))
           (slot "type" (value (plotly-type geometrics)))
           (slot "name" (value (and aesthetics (label aesthetics))))
           (slot "marker"
                 (object
                   #1=(cond
                        (color (slot "color" (var color)))
                        (aesthetics (slot "color" (value (color aesthetics)))))
                   (when size
                     (slot "size" (var size)))
                   (when label
                     (slot "text" (var label)))
                   (slot "textposition"
                         (value (and aesthetics (label-position aesthetics))))))
           (slot "list" (object #1#))))))))


(defun plotly-generate-data (stack)
  (iterate
    (with geometrics = (geometrics-layers stack))
    (with index.table = (cons 0 (make-hash-table)))
    (for g in geometrics)
    (for i from 1)
    (collect (plotly-generate-data/impl stack g index.table i)
      into data-forms)
    (finally (return (values data-forms
                             (~> index.table
                                 cdr
                                 hash-table-values))))))


(defgeneric plotly-generate-facets (facets))


(defmethod plotly-generate-facets ((facets (eql nil)))
  (list nil nil))


(defmethod plotly-generate-facets ((facets grid-layer))
  (list "grid"
        (with-output-to-string (stream)
          (json (stream)
            (object
              (slot "pattern" (value "independent"))
              (slot "rows" (value (rows facets)))
              (slot "columns" (value (columns facets))))))))


(defun plotly-format-axis (facets mapping axis number)
  (when axis
    (object
      (slot "title"
            (object
              (cond ((and axis (label axis))
                     (plotly-format-no-nulls "text" (label axis)))
                    (mapping
                     (plotly-format-no-nulls "text" mapping)))))
      (slot "scaleanchor" (if facets
                              (valuel (scale-anchor axis) number)
                              (value (scale-anchor axis))))
      (slot "range" (value (range axis)))
      (slot "constrain" (value (constrain axis)))
      (slot "scaleratio" (value (scale-ratio axis)))
      (slot "ticklen" (value (tick-length axis)))
      (slot "dtick" (value (dtick axis))))))


(defun plotly-generate-layout (stack)
  (bind ((aesthetics (aesthetics-layer stack))
         (facets (facets-layer stack))
         ((slot facets-object) (plotly-generate-facets facets))
         (xaxis (x aesthetics))
         (yaxis (y aesthetics))
         (geometrics (geometrics-layers stack))
         (facets (facets-layer stack)))
    (with-output-to-string (stream)
      (json (stream)
        (object
          (when slot
            (slot slot (format *json-stream* facets-object)))
          (slot "height" (value (height aesthetics)))
          (slot "width" (value (width aesthetics)))
          (slot "title" (value (label aesthetics)))
          (iterate
            (for g in geometrics)
            (for i from 1)
            (for mapping = (read-mapping g))
            (slot (if (null facets)
                      "xaxis"
                      (format nil "xaxis~a" i))
                  (plotly-format-axis facets
                                      (and mapping (x mapping))
                                      xaxis
                                      i))
            (slot (if (null facets)
                      "yaxis"
                      (format nil "yaxis~a" i))
                  (plotly-format-axis facets
                                      (and mapping (y mapping))
                                      yaxis
                                      i))))))))



(defun plotly-visualize (stack stream)
  (bind ((layout (plotly-generate-layout stack))
         ((:values data-forms variables) (plotly-generate-data stack)))
    (format stream "<html>~%")
    (format stream "<head>~%")
    (format stream "<script src='https://cdn.plot.ly/plotly-latest.min.js'></script></head>~%")
    (format stream "<body>")
    (format stream
            "<div id='plotDiv'><!-- Plotly chart will be drawn inside this DIV --></div>~%")
    (format stream "<script type='text/javascript'>~%")
    (iterate
      (for (name . value) in variables)
      (format stream "var ~a = ~a;~%" name value))
    (format stream "var data = [~{~a~^,~}];~%~%" data-forms)
    (format stream "var layout = ~a;~%~%" layout)
    (format stream "Plotly.newPlot('plotDiv', data, layout);~%")
    (format stream "</script>~%")
    (format stream "</body>")
    (format stream "</html>~%")))
