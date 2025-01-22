(asdf:defsystem #:lab4
  :DEPENDS-ON (#:cl-ascii-table
               #:alexandria
               #:tble-cnst)
  :serial t
  :components ((:file "package")
               (:file "main")))

