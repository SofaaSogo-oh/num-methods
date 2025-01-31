(asdf:defsystem #:lab1
  :DEPENDS-ON (#:cl-ascii-table
               #:tble-cnst
               #:function-cache
               #:cl-ppcre)
  :serial t
  :COMPONENTS ((:file "package")
               (:file "first-newthon")
               (:file "second-newthon")
               (:file "lagrange")
               (:file "main")))
