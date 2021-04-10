(cl:in-package #:vellum-plot)


(defun aesthetics (&key x y color shape size label label-position
                        width height)
  (make 'aesthetics-layer
        :x x
        :y y
        :width width
        :height height
        :label label
        :label-position label-position
        :color color
        :shape shape
        :size size))


(defun mapping (&key x y z color shape size label label-position)
  (make 'mapping
        :x x
        :y y
        :z z
        :label label
        :label-position label-position
        :color color
        :shape shape
        :size size))


(defun scale ()
  cl-ds.utils:todo)


(defun points (&key mapping aesthetics)
  (make 'points-geometrics
        :aesthetics aesthetics
        :mapping mapping))


(defun grid (&key rows columns)
  (make 'grid-layer
        :rows rows
        :columns columns))


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


(defun statistics ()
  cl-ds.utils:todo)


(defun coordinates ()
  cl-ds.utils:todo)


(defun stack (data-layer &rest layers)
  (reduce #'add layers :initial-value data-layer))


(defun axis (&key scale-anchor label
                  dtick tick-length constrain
                  scale-ratio range)
  (make 'axis
        :scale-ratio scale-ratio
        :tick-length tick-length
        :scale-anchor scale-anchor
        :constrain constrain
        :range range
        :label label
        :dtick dtick))
