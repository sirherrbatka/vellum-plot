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
                (let ((content (vellum:rr column)))
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
    nil)
  (:method ((geometrics box-geometrics) mapping)
    nil))


(defgeneric plotly-type (geometrics)
  (:method ((geometrics points-geometrics))
    "scatter")
  (:method ((geometrics heatmap-geometrics))
    "heatmap")
  (:method ((geometrics box-geometrics))
    "box")
  (:method ((geometrics bar-geometrics))
    "bar")
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

(defun plotly-generate-data/impl (stack geometrics index.table ix iy)
  (bind ((mapping (read-mapping geometrics))
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
           (slot "xaxis" (if (= ix 1)
                             (value "x")
                             (valuel :x ix)))
           (slot "yaxis" (if (= iy 1)
                             (value "y")
                             (valuel :y iy)))
           (slot "x" (var x))
           (slot "y" (var y))
           (unless (null z)
             (slot "z" (var z)))
           (slot "mode" (value (plotly-mode geometrics mapping)))
           (slot "type" (value (plotly-type geometrics)))
           (slot "name" (value (or (and aesthetics (label aesthetics))
                                   (y mapping))))
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


(defun plotly-generate-data (stack axis-mapping)
  (iterate
    (with xmapping = (xmapping axis-mapping))
    (with ymapping = (ymapping axis-mapping))
    (with geometrics = (geometrics-layers stack))
    (with index.table = (cons 0 (make-hash-table)))
    (for g in geometrics)
    (for mapping = (read-mapping g))
    (for ix = (gethash mapping xmapping))
    (for iy = (gethash mapping ymapping))
    (collect (plotly-generate-data/impl stack g index.table ix iy)
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


(defclass axis-mapping ()
  ((%xmapping :initarg :xmapping
              :reader xmapping)
   (%ymapping :initarg :ymapping
              :reader ymapping)
   (%groups :initarg :groups
            :reader groups)
   (%geometric-numbers :initarg :geometric-numbers
                       :reader geometric-numbers)
   (%xaxis :initarg :xaxis
           :reader xaxis)
   (%yaxis :initarg :yaxis
           :reader yaxis)))


(defun xaxis-for-group (axis-mapping group)
  (gethash group
           (xaxis axis-mapping)
           (gethash nil (xaxis axis-mapping))))


(defun yaxis-for-group (axis-mapping group)
  (gethash group
           (yaxis axis-mapping)
           (gethash nil (yaxis axis-mapping))))


(defun axis-mapping (stack)
  (let* ((aesthetics (aesthetics-layer stack))
         (xaxis (x aesthetics))
         (yaxis (y aesthetics))
         (facets (facets-layer stack))
         (geometrics (geometrics-layers stack))
         (groups
           (~> (list geometrics xaxis yaxis)
               cl-ds.alg:multiplex
               (cl-ds.alg:on-each #'group)
               (cl-ds.alg:without #'null)
               (cl-ds.alg:enumerate :number 1
                                    :test 'equal)))
         (geometric-numbers
           (~> geometrics
               (cl-ds.alg:only #'null :key #'group)
               (cl-ds.alg:enumerate
                :number (~> groups hash-table-values (extremum #'>) (or 0) 1+)
                :test 'eq)))
         (xresult (make-hash-table :test 'eq))
         (yresult (make-hash-table :test 'eq))
         (xaxis-map (cl-ds.alg:to-hash-table
                     xaxis
                     :test 'equal
                     :hash-table-key (compose
                                      (rcurry #'gethash groups)
                                      #'group)))
         (yaxis-map (cl-ds.alg:to-hash-table
                     yaxis
                     :test 'equal
                     :hash-table-key (compose
                                      (rcurry #'gethash groups)
                                      #'group))))
    (iterate
      (for g in geometrics)
      (for mapping = (read-mapping g))
      (if (null facets)
          (setf (gethash mapping xresult) 1
                (gethash mapping yresult) 1)
          (let ((number (or (gethash (group g) groups)
                            (gethash g geometric-numbers))))
            (ensure (gethash mapping xresult)
              number)
            (ensure (gethash mapping yresult)
              number))))
    (make 'axis-mapping
          :xmapping xresult
          :ymapping yresult
          :groups groups
          :xaxis xaxis-map
          :yaxis yaxis-map
          :geometric-numbers geometric-numbers)))


(defun group-number (axis-mapping group)
  (gethash group (groups axis-mapping) 1))


(defun plotly-format-axis (mapping axis number)
  (when axis
    (object
      (slot "title"
            (object
              (cond ((and axis (label axis))
                     (plotly-format-no-nulls "text" (label axis)))
                    (mapping
                     (plotly-format-no-nulls "text" mapping)))))
      (slot "scaleanchor" (if (= number 1)
                              (value (scale-anchor axis))
                              (valuel (scale-anchor axis) number)))
      (slot "range" (value (range axis)))
      (slot "constrain" (value (constrain axis)))
      (slot "scaleratio" (value (scale-ratio axis)))
      (slot "ticklen" (value (tick-length axis)))
      (slot "dtick" (value (dtick axis))))))


(defun plotly-generate-layout (stack axis-mapping)
  (bind ((aesthetics (aesthetics-layer stack))
         (facets (facets-layer stack))
         ((slot facets-object) (plotly-generate-facets facets)))
    (with-output-to-string (stream)
      (json (stream)
        (object
          (when slot
            (slot slot (format *json-stream* facets-object)))
          (slot "height" (value (height aesthetics)))
          (slot "width" (value (width aesthetics)))
          (slot "title" (value (label aesthetics)))
          (iterate
            (with seen = (make-hash-table))
            (for (mapping number) in-hashtable (xmapping axis-mapping))
            (for seen? = (shiftf (gethash number seen) t))
            (when seen? (next-iteration))
            (slot (if (= number 1)
                      "xaxis"
                      (format nil "xaxis~a" number))
                  (plotly-format-axis (x mapping)
                                      (xaxis-for-group axis-mapping number)
                                      number)))
          (iterate
            (with seen = (make-hash-table))
            (for (mapping number) in-hashtable (ymapping axis-mapping))
            (for seen? = (shiftf (gethash number seen) t))
            (when seen? (next-iteration))
            (slot (if (= number 1)
                      "yaxis"
                      (format nil "yaxis~a" number))
                  (plotly-format-axis (y mapping)
                                      (yaxis-for-group axis-mapping number)
                                      number))))))))


(defun plotly-visualize (stack stream)
  (bind ((axis-mapping (axis-mapping stack))
         (layout (plotly-generate-layout stack axis-mapping))
         ((:values data-forms variables) (plotly-generate-data stack axis-mapping)))
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


(defun labeled (data label)
  (~>> data
       plist-alist
       (remove-if-not (curry #'eq label) _ :key #'car)
       (mapcar #'cdr)))
