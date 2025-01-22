(asdf:defsystem #:lab2
  :DEPENDS-ON (#:cl-ascii-table
               #:alexandria
               #:tble-cnst)
  :serial t
  :components ((:file "package")
               (:file "poly")
               (:file "first-newthon")
               (:file "second-newthon")
               (:file "main")))

