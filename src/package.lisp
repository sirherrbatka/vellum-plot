(cl:in-package #:cl-user)


(defpackage #:vellum-plot
  (:use #:cl #:vellum.aux-package)
  (:shadow #:box)
  (:export
   #:aesthetics
   #:points
   #:stack
   #:line
   #:missing-required-slot
   #:mapping
   #:grid
   #:box
   #:heatmap
   #:axis
   #:add))
