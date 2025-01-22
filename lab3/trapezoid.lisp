(in-package #:num-methods.lab3)

(defun trapezoid (f nde)
  (let ((h (nde-h nde))
        (n (nde-n nde)))
    (* h
       (- (reduce #'+ (mapcar
                        (compose f (curry #'nde-i nde))
                        (iota (1+ n))))
          (/ (+ (nde-beg nde)
                (nde-end nde))
            2)))))
