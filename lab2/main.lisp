(in-package #:num-methods.lab2)

(defun target-f (x)
  (* 27 (sqrt x) (cos (+ (sqrt (* x x x)) 8))))

(defparameter *target-nde* (make-nde-t
                             :beg 3.35d0
                             :end 3.36d0
                             :n 5))

(reverse (mapcar #'abs (generate-derived-polynom-qsts 1)))
(second-newthon.prod 2 2)

(first-newthon.x_0 *TARGET-NDE* 3.355)
(second-newthon.x_n *TARGET-NDE* 3.355d0)
(mapcar (curry #'nde-i *TARGET-NDE*) (iota (1+ (nde-n *target-nde*))))

(defun main ()
  (let ((table (ascii-table:make-table *interpl-table-deriv* :header "Приближенное вычислкение производной"))
        (total_points (insert_natural "Введите число точек: ")))
    (dotimes (i total_points)
      (let* ((x (insert_from_range (format nil "Введите точку x_~A: " i) *TARGET-NDE*))
             (f<x> (target-f x))
             (PI<x> (first-newthon.beg *TARGET-NDE* #'target-f x))
             (dPI<x> (abs (- f<x> PI<x>)))
             (PI_<x> (first-newthon.near *TARGET-NDE* #'target-f x))
             (dPI_<x> (abs (- f<x> PI_<x>)))
             (PII<x> (second-newthon.end *TARGET-NDE* #'target-f x))
             (dPII<x> (abs (- f<x> PII<x>)))
             (PII_<x> (second-newthon.near *TARGET-NDE* #'target-f x))
             (dPII_<x> (abs (- f<x> PII_<x>))))
        (ascii-table:add-row table (list x f<x>
                                         PII<x> dPII<x>
                                         PII_<x> dPII_<x>))))
    (ascii-table:display table)))

