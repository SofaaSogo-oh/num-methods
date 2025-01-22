(in-package #:num-methods.lab2)

(defun generate-polynom-qsts (n)
  "This function defines the function, that return list of polynomial multipliers q^[n]_k, k = 1,n"
  (labels ((generate-polynom-qsts_ (k res_k)
             (if (> k n) res_k
               (let* ((k1 (1+ k))
                      (res_k1 (mapcar #'+
                                (append res_k '(0))
                                (cons
                                  0
                                  (mapcar
                                    (curry #'* (- k))
                                    res_k)))))
                 (generate-polynom-qsts_ k1 res_k1)))))
    (generate-polynom-qsts_ 1 '(1))))

(defun generate-derived-polynom-qsts (n)
  (mapcar #'*
          (generate-polynom-qsts n)
          (reverse (iota (1+ n) :start 1))))
