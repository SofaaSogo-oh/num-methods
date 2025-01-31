(in-package #:num-methods.lab1)

(defun lagrange.q (nde x)
  (/ (- x (nde-beg nde))
     (nde-h nde)))

(defun lagrange.c_i (nde f i)
  (/ (funcall f (nde-i nde i))
     (reduce #'*
             (mapcar
               #'(lambda (j) (progn
                               (if (= j i) 1 (- (nde-i nde i)
                                                (nde-i nde j)))))
               (iota (1+ (nde-n nde)))))))

(defun lagrange.c (nde f)
  (let ((n (nde-n nde)))
    (loop for i from 0 to n 
          collect (lagrange.c_i nde f i))))

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

(defun lagrange-2 (nde f x)
  (let* ((n (nde-n nde))
         (l_ (curry #'lagrange.c_i nde f))
         (x_ (curry #'nde-i nde)))
    (reduce #'+
            (loop for i from 0 to n collect
                  (* (funcall l_ i)
                     (reduce 
                       #'*
                       (loop for j from 0 to n collect
                             (if (= i j) 1
                                 (- x (funcall x_ j)))))))))) 
