(asdf:defsystem #:lab3
  :DEPENDS-ON (#:cl-ascii-table
               #:alexandria
               #:tble-cnst)
  :serial t
  :components ((:file "package")
               (:file "trapezoid")
               (:file "simpson")
               (:file "gauss")
               (:file "main")))

