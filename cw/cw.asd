(asdf:defsystem #:cw
  :DEPENDS-ON (#:cl-ascii-table
               #:alexandria
               #:tble-cnst
               #:statistics)
  :serial t
  :components ((:file "package")
               (:file "display-distr")
               (:file "norm-distr")
               (:file "main")))

