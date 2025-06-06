(in-package #:num-methods.lab2)

(defun second-newthon.p (nde x x_n)
  (/ (- x x_n)
     (nde-h nde)))
  
(defun second-newthon.x_n (nde x)
  (reduce #'(lambda (n1 n2)
              (if (< (abs (- n1 x)) (abs (- n2 x)))
                  n1 n2))
          (mapcar (curry #'nde-i nde)
                  (iota (1+ (nde-n nde))))))

(defun second-newthon.prod (n q)
    (labels ((first-newthon-prod_ (prod_k sum_k qts_k)
               (if (null qts_k) sum_k
                   (let* ((prod_k1 (* prod_k q))
                          (sum_k1 (+ sum_k (* prod_k (car qts_k))))
                          (qts_k1 (cdr qts_k)))
                     (format t "(+ ~a (* ~a ~a))~%" sum_k prod_k  qts_k)
                     (first-newthon-prod_  prod_k1 sum_k1 qts_k1)))))
      (first-newthon-prod_ 1 0 
                           (mapcar #'abs
                             (reverse 
                               (generate-derived-polynom-qsts n))))))


(defun second-newthon.meta (nde f x x_n)
  (let ((n (nde-n nde))
        (p (second-newthon.p nde x x_n)))
    (labels ((second-newthon_ (k qts_k sum_k)
               (if (> k n) (/ sum_k (nde-h nde))
                   (let* ((k1 (1+ k))
                          (qts_k1 (* qts_k k))
                          (sum_k1 (+ sum_k
                                     (/ (* (second-newthon.prod (1- k) p)
                                           (finite-subs 
                                             f (nde-i nde (- n k))
                                             k nde))
                                        qts_k))))
                     (format t "~a ~a~%" (finite-subs f (nde-i nde (- n k)) k nde) qts_k)
                     (second-newthon_ k1 qts_k1 sum_k1)))))
      (format t "x_n: ~a ~%t: ~a~%" x_n p)
      (second-newthon_ 1 1 0))))

(defun second-newthon.end (nde f x)
  (second-newthon.meta nde f x (nde-end nde)))

(defun second-newthon.near (nde f x)
  (second-newthon.meta nde f x (second-newthon.x_n nde x)))

