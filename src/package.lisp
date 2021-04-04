(cl:in-package #:cl-user)


(defpackage #:vellum-plot
  (:use #:cl #:vellum.aux-package)
  (:export
   #:aesthetics
   #:points
   #:stack
   #:line
   #:missing-required-slot
   #:mapping
   #:heatmap
   #:axis
   #:add))
