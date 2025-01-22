(in-package #:num-methods.tble-cnst)

(defun combinations_cnt (n k)
  (labels ((ccnt_tmp (i prod_i)
             (if (> i (- n k)) prod_i
                 (let ((i1 (1+ i))
                       (prod_i1 (* prod_i (/ (+ i k)
                                           i))))
                   (ccnt_tmp i1 prod_i1)))))
    (ccnt_tmp 1 1)))


