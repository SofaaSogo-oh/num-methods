(in-package #:num-methods.lab2)

(defun first-newthon.q (nde x x_0)
  (/ (- x x_0)
    (nde-h nde)))

(defun first-newthon.x_0 (nde x)
  (reduce #'(lambda (n1 n2)
              (if (<= (abs (- n1 x)) (abs (- n2 x)))
                  n1 n2))
            (mapcar (curry #'nde-i nde) 
                    (iota (1+ (nde-n nde))))))

(defun first-newthon-prod (n q)
    (labels ((first-newthon-prod_ (prod_k sum_k qts_k)
               (if (null qts_k) sum_k
                   (let* ((prod_k1 (* prod_k q))
                          (sum_k1 (+ (* prod_k (car qts_k))))
                          (qts_k1 (cdr qts_k)))
                     (first-newthon-prod_  prod_k1 sum_k1 qts_k1)))))
      (first-newthon-prod_ 1 0 
                           (reverse 
                             (generate-derived-polynom-qsts n)))))

(defun first-newthon.meta (nde f x x_0) 
  (let ((n (nde-n nde))
        (q (first-newthon.q nde x x_0)))
    (labels ((first-newthon_ (k qts_k sum_k)
               (if (> k n) sum_k
                 (let* ((k1 (1+ k))
                        (qts_k1 (* qts_k k1))
                        (sum_k1 (+ sum_k
                                   (/ (* (first-newthon-prod k q) 
                                         (finite-subs f x_0 k nde)) 
                                    qts_k1))))
                   (first-newthon_ k1 qts_k1 sum_k1)))))
      (first-newthon_ 0 1 0))))

(defun first-newthon.beg (nde f x)
  (first-newthon.meta nde f x (nde-beg nde)))

(defun first-newthon.near (nde f x)
  (first-newthon.meta nde f x (first-newthon.x_0 nde x)))
