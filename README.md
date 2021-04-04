# vellum-plot

This system adds gramar-of-graphics style of plotting functionality for the vellum tables. Currently, this is work in progress.


## Defining plots
In a true gramar of graphics fashion, plots are defined using the layerd approach. The stack always starts with the data layer, being actually the vellum data frame. On top of that aesthetics and mappings are defined.

To demonstrate, the following example defines ROC plot, demonstrating balance of TPR and FPR values from a test.

```
(defparameter *plot*
  (vellum-plot:stack
   frame
   (vellum-plot:aesthetics
    :label "ROC"
    :width 800
    :height 800
    :x (vellum-plot:axis
        :dtick 0.1
        :range '(0.0 1.0)
        :label "FPR"
        :constrain :domain)
    :y (vellum-plot:axis
        :dtick 0.1
        :range '(0.0 1.0)
        :scale-anchor :x
        :label "Metrics"
        :scale-ratio 1))
   (vellum-plot:line
    :mapping (vellum-plot:mapping :x "FPR" :y "TPR")
    :aesthetics (vellum-plot:aesthetics :label "TPR"))))
```

## Visualizing plots
Defined plot can be visualized using the vellum:visualize function. In theory, various plotting backends could be implemented, in practice though only the :plotly backend exists at the moment. Plotly plots are just html files with a lot of javascript: portable, interactive, and useful file format.

```
(vellum:visualize :plotly *plot* destination-path)
```

## Other options
In addition to the plain line plots, heatmaps and points are also supported. Hopefully more types of plots can be added in the future.


## Future work
As already stated this system is very much the work in progress. For one, the error handling should be improved. As for the extra features, well: more backends, more plot types.
