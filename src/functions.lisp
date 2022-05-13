(cl:in-package #:vellum-plot)


(defun aesthetics (&rest axis
                   &key color shape size label label-position
                     bargap
                     width height mode &allow-other-keys)
  (make 'aesthetics-layer
        :x (labeled axis :x)
        :y (labeled axis :y)
        :width width
        :bargap bargap
        :mode mode
        :height height
        :label label
        :label-position label-position
        :color color
        :shape shape
        :size size))


(defun mapping (&key x y z color shape size label)
  (make 'mapping
        :x x
        :y y
        :z z
        :label label
        :color color
        :shape shape
        :size size))


(defun scale ()
  cl-ds.utils:todo)


(defun points (&key mapping aesthetics group)
  (make 'points-geometrics
        :group group
        :aesthetics aesthetics
        :mapping mapping))


(defun grid (&key rows columns)
  (make 'grid-layer
        :rows rows
        :columns columns))


(defun barmode (&key mode)
  (make 'barmode-layer
        :mode mode))


(defun line (&key mapping aesthetics group)
  (make 'line-geometrics
        :group group
        :aesthetics aesthetics
        :mapping mapping))


(defun heatmap (&key mapping aesthetics group)
  (make 'heatmap-geometrics
        :group group
        :aesthetics aesthetics
        :mapping mapping))


(defun box (&key mapping aesthetics group)
  (make 'box-geometrics
        :group group
        :aesthetics aesthetics
        :mapping mapping))


(defun bar (&key mapping aesthetics group)
  (make 'bar-geometrics
        :group group
        :aesthetics aesthetics
        :mapping mapping))


(defun statistics ()
  cl-ds.utils:todo)


(defun coordinates ()
  cl-ds.utils:todo)


(defun stack (data-layer &rest layers)
  (reduce #'add layers :initial-value data-layer))


(defun axis (&key scale-anchor label
                  dtick tick-length constrain
                  scale-ratio range group)
  (make 'axis
        :group group
        :scale-ratio scale-ratio
        :tick-length tick-length
        :scale-anchor scale-anchor
        :constrain constrain
        :range range
        :label label
        :dtick dtick))
