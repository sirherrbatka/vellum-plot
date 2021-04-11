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
   (:description "Specify line plot. You must provide the mapping argument to establish mapping of the data layer to the plot itself."))

  (function
   mapping
   (:description "Constructs mapping layer. Used for mapping the data layer to the geometrics."))
   )
