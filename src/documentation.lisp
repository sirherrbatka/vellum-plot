(cl:in-package #:vellum-plot)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
   data-layer
   (:description "Read the data layer slot from the STACK object."))

  (function
   aesthetics-layer
   (:description "Read the aesthetics layer from the STACK object."))

  (function
   aesthetics
   (:description "Construct the aesthetics layer from the supplied arguments."
    :notes "Can be used to define multiple axis if one then one graph is written using the facets."))

  (function
   line
   (:description "Specify line geometrics. You must provide the mapping argument to establish mapping of the data layer to the x, y axis."
    :notes "All geometrics can be grouped in order to draw subplots."))

  (function
   heatmap
   (:description "Specify line geometrics. You must provide the mapping argument to establish mapping of the data layer to x, y, z axis."
    :notes "All geometrics can be grouped in order to draw subplots.")
   )

  (function
   box
   (:description "Specify box geometrics. You must provide the mapping argument to establish mapping of the data layer to x, y axis."
    :notes "All geometrics can be grouped in order to draw subplots."))

  (function
   mapping
   (:description "Constructs mapping layer. Used for mapping the data layer to the geometrics."))

  (function
   axis
   (:description "Constructs the axis specification. Axis can be grouped in order to draw subplots.")))
