(asdf:defsystem #:tble-cnst
  :serial t
  :depends-on (#:ALEXANDRIA)

  :COMPONENTS ((:file "package")
               (:file "dialogs")
               (:file "cmf")
               (:file "nde")
               (:file "interpl")
               (:file "int")))
