(in-package #:num-methods.lab3)

(defun target-f (x) 
  (exp (/ x 3)))

(defun target-prt-int-f (x)
  (* 3 (target-f x)))

(defparameter *target-val*
  (- (target-prt-int-f 3)
     (target-prt-int-f 2)))

(defun add-row-int (table target)
  (let* ((n (nde-n target))
         (y *TARGET-VAL*)
         (y_t (trapezoid #'target-f target)) (dy_t (abs (- y_t y)))
         (y_s (simpson #'target-f target))   (dy_s (abs (- y_s y)))
         (y_g (gauss #'target-f target))     (dy_g (abs (- y_g y))))
    (ascii-table:add-row table (list n y
                                     y_t dy_t
                                     y_s dy_s
                                     y_g dy_g))))

(defparameter *target-nde4* (make-nde-t
                              :beg 2
                              :end 3
                              :n 4))

(defparameter *target-nde8* (make-nde-t
                              :beg 2
                              :end 3
                              :n 8))

(defun main ()
  (let ((table (ascii-table:make-table *INT-TABLE* :header "Методы численного интегрирования")))
    (add-row-int table *TARGET-NDE4*)
    (add-row-int table *TARGET-NDE8*)
    (ascii-table:display table)))
