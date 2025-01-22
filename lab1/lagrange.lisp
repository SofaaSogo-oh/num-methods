(in-package #:num-methods.lab1)

(defun lagrange.q (nde x)
  (/ (- x (nde-beg nde))
     (nde-h nde)))

(defun lagrange.c (nde f)
  (let ((n (nde-n nde)))
    (loop for i from 0 to n 
          collect (/ (funcall f (nde-i nde i))
                     (reduce #'*
                             (mapcar
                               #'(lambda (j) (if (= j i) 1 (- (nde-i nde i)
                                                              (nde-i nde j))))
                               (iota n)))))))

(defun lagrange (nde f x)
  (let ((n (nde-n nde))
        (q (lagrange.q nde x)))
    (labels ((lag_prod (i j res)
               (if (> j n) res
                   (let* ((j1 (1+ j))
                          (res1 (if (= i j) res
                                    (* res (/ (- q j)
                                            (- i j))))))
                     (lag_prod i j1 res1))))
             (lag_sum (i res)
               (if (> i n) res
                   (let* ((i1 (1+ i))
                          (res1 (+ res (* (funcall f (nde-i nde i))
                                          (lag_prod i 0 1)))))
                     (lag_sum i1 res1)))))
      (lag_sum 0 0))))
