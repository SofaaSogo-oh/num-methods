(in-package #:num-methods.lab1)

(defun second-newthon.p (nde x)
  (/ (- x (nde-end nde))
     (nde-h nde)))

(defun second-newthon (nde f x)
  (let ((n (nde-n nde))
        (p (second-newthon.p nde x)))
    (labels ((second-newthon_ (k qnt_k sum_k)
              (if (> k n) sum_k
                  (let* ((k1 (1+ k))
                         (qnt_k1 (* qnt_k (/ (+ p k)
                                           k1)))
                         (sum_k1 (+ sum_k (* qnt_k 
                                           (finite-subs 
                                             f (nde-i nde (- n k))
                                             k nde)))))
                    (second-newthon_ k1 qnt_k1 sum_k1)))))
      (format t "t: ~a~%" p)
      (second-newthon_ 0 1 0))))

