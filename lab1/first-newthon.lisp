(in-package #:num-methods.lab1)

(defun first-newthon.q (nde x)
  (/ (- x (nde-beg nde))
    (nde-h nde)))

(defun first-newthon (nde f x)
  (let ((n (nde-n nde))
        (q (first-newthon.q nde x))
        (x_0 (nde-beg nde)))
    (labels ((first-newthon_ (k qnt_k sum_k)
               (if (> k n) sum_k
                   (let* ((k1 (1+ k))
                          (qnt_k1 (* qnt_k (/ (- q k)
                                            k1)))
                          (sum_k1 (+ sum_k (* qnt_k (finite-subs f x_0 k nde)))))
                    (first-newthon_ k1 qnt_k1 sum_k1)))))
      (first-newthon_ 0 1 0))))

