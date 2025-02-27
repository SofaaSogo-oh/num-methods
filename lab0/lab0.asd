(asdf:defsystem #:lab0
  :DEPENDS-ON (#:cl-ascii-table
               #:tble-cnst
               #:function-cache
               #:cl-ppcre)
  :serial t
  :COMPONENTS ((:file "package")
               (:file "main")))
