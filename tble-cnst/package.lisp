(defpackage #:num-methods.tble-cnst
  (:use #:cl)
  (:IMPORT-FROM #:ALEXANDRIA #:iota)
  (:export #:*interpl-table*
           #:*interpl-table-deriv*)
  (:export #:*int-table*
          #:*iint-table*)
  (:export #:insert_dialog
             #:insert_natural)
  (:export #:nde-t
             #:make-nde-t
             #:nde-beg
             #:nde-end
             #:nde-n
             #:nde-h
             #:nde-i
             #:insert_from_range)
  (:export #:combinations_cnt
           #:finite-subs))

