(asdf:defsystem vellum-plot
  :name "vellum-plot"
  :version "1.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :depends-on ( #:iterate
                #:serapeum
                (:version #:vellum ((>= "1.2.0")))
                #:alexandria
                #:documentation-utils-extensions)
  :serial T
  :pathname "src"
  :components ((:file "package")
               (:file "variables")
               (:file "generics")
               (:file "conditions")
               (:file "types")
               (:file "internal")
               (:file "functions")
               (:file "implementation")
               (:file "documentation")))
