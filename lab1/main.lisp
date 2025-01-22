(in-package #:num-methods.lab1)


(defparameter *target-nde* (make-nde-t
                             :beg 3.35d0
                             :end 3.36d0
                             :n 6))

(defun target-f (x)
  (* 18
     (sin (+ (sqrt (* x x x))
             8))))

(defun disp-finite-subs (nde f)
  (let* ((n (nde-n nde))
         (h (nde-h nde))
         (a (nde-beg nde))
         (table (ascii-table:make-table 
                  (loop for i from 0 to n collect
                    (format nil "x_~A" i)) 
                  :header "Таблица конечных разностей")))
    (ascii-table:add-row table (loop for k from 0 to n collect (nde-i nde k)))
    (ascii-table:add-separator table)
    (loop for m from 0 to n do
      (ascii-table:add-row table
        (loop for k from 0 to n collect
          (if (<= k (- n m))
            (finite-subs f (+ a (* h k)) m nde)
            ""))))
    (ascii-table:display table)))


(defun main ()
  (let ((table (ascii-table:make-table *INTERPL-TABLE* :header "Сравнение методов интерполяции"))
        (total_points (insert_natural "Введите число точек: ")))
    (dotimes (i total_points)
      (let* ((x (insert_from_range (format nil "Введите точку x_~A: " i) *TARGET-NDE*))
             (f<x> (target-f x))
             (PI<x> (first-newthon *TARGET-NDE* #'target-f x))
             (dPI<x> (abs (- f<x> PI<x>)))
             (PII<x> (second-newthon *TARGET-NDE* #'target-f x))
             (dPII<x> (abs (- f<x> PII<x>)))
             (L<x> (lagrange *TARGET-NDE* #'target-f x))
             (dL<x> (abs (- f<x> L<x>))))
        (ascii-table:add-row table (list x f<x>
                                         PI<x> dPI<x>
                                         PII<x> dPII<x>
                                         L<x> dL<x>))))
    (ascii-table:display table)))

(defun lab1-demo nil
  (disp-finite-subs *TARGET-NDE* #'target-f)
  (format t "~A~%" (lagrange.c *TARGET-NDE* #'target-f))
  (main))
  
(lab1-demo)

