(in-package #:num-methods.lab2)

(defun target-f (x)
  (* 27 (sqrt x) (cos (+ (sqrt (* x x x)) 8))))

(defun target-f-src (x)
  (* 18
     (sin (+ (sqrt (* x x x))
             8))))

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
  (let ((table (ascii-table:make-table '("x" "f'(x)" " ̅x₁" "Pᴵₙ(x)" "ΔPᴵₙ(x)" " ̅x" "P𝕀ₙ(x)" "ΔP𝕀ₙ(x)") :header "Приближенное вычислкение производной"))
        (total_points (insert_natural "Введите число точек: ")))
    (dotimes (i total_points)
      (let* ((x (insert_from_range (format nil "Введите точку x_~A: " i) *TARGET-NDE*))
             (f<x> (target-f x))
             (PI_x0 (first-newthon.x_0 *TARGET-NDE* x))
             (PI_<x> (first-newthon.near *TARGET-NDE* #'target-f-src x))
             (dPI_<x> (abs (- f<x> PI_<x>)))
             (PII_x0 (second-newthon.x_n *TARGET-NDE* x))
             (PII_<x> (second-newthon.near *TARGET-NDE* #'target-f-src x))
             (dPII_<x> (abs (- f<x> PII_<x>))))
        (ascii-table:add-row table (list x f<x>
                                         PI_X0 PI_<x> dPI_<x>
                                         PII_X0 PII_<x> dPII_<x>))))
    (ascii-table:display table)))

(labels ((sign (x) (format nil "~:[~;+~]~a" (> x 0) x))
         (frac (x y) (format nil "\\frac{~a}{~a}" x y))
         (dlta (m k) (format nil "\\Delta~:[^~a~;~] y_~a" (= m 1) m k))
         (q^k (k) (if (> k 0) (format nil "q~:[^~a~;~]" (= k 1) k) ""))
         (t^k (k) (if (> k 0) (format nil "t~:[^~a~;~]" (= k 1) k) ""))
         (fact (n) (format nil "~a!" n))
         (concat (&rest args) (format nil "~{~a~}" args)))
  (let* ((n (1- 5))
         (muls (loop for i from 0 to n 
                     collect 
                     (mapcar #'sign
                             (generate-derived-polynom-qsts i))))
         (qmuls (loop for i from 0 to n
                      collect
                      (mapcar #'q^k 
                       (reverse (loop for j from 0 to i collect j)))))
         (upper-frac (mapcar (compose 
                              (curry #'apply #'concat)
                              (curry #'mapcar #'concat))
                             muls qmuls))
         (under-frac (loop for i from 0 to n collect (fact i)))
         (fractions (mapcar #'frac upper-frac under-frac))
         (dltas (loop for i from 1 to (1+ n) collect (dlta i 0)))
         (dltas-in-nums (loop for i from 1 to (1+ n) collect 
                              (sign (finite-subs #'target-f-src (nde-beg *target-nde*) i *target-nde*)))))
    (format t "~a[~a]~%" (frac 1 "h")
      (format nil "~{~a~^+~}" (mapcar #'concat fractions dltas)))
    (format t "~a[~a]~%" (frac 1 (nde-h *target-nde*))
            (apply #'concat (mapcar #'concat dltas-in-nums fractions)))))


(labels ((sign (x) (format nil "~:[~;+~]~a" (> x 0) x))
         (frac (x y) (format nil "\\frac{~a}{~a}" x y))
         (dlta (m k) (format nil "\\Delta~:[^~a~;~] y_~a" (= m 1) m k))
         (q^k (k) (if (> k 0) (format nil "q~:[^~a~;~]" (= k 1) k) ""))
         (t^k (k) (if (> k 0) (format nil "t~:[^~a~;~]" (= k 1) k) ""))
         (fact (n) (format nil "~a!" n))
         (concat (&rest args) (format nil "~{~a~}" args)))
  (let* ((n (1- 5))
         (muls (loop for i from 0 to n 
                     collect 
                     (mapcar (compose #'sign #'abs)
                             (generate-derived-polynom-qsts i))))
         (qmuls (loop for i from 0 to n
                      collect
                      (mapcar #'q^k 
                       (reverse (loop for j from 0 to i collect j)))))
         (upper-frac (mapcar (compose 
                              (curry #'apply #'concat)
                              (curry #'mapcar #'concat))
                             muls qmuls))
         (under-frac (loop for i from 0 to n collect (fact i)))
         (fractions (mapcar #'frac upper-frac under-frac))
         (dltas (loop for i from 1 to (1+ n) collect (dlta i (- (1+ n) i))))
         (dltas-in-nums (loop for i from 1 to (1+ n) collect 
                              (sign (finite-subs #'target-f-src (nde-i *TARGET-NDE* (- (1+ n) i)) i *TARGET-NDE*)))))
    (format t "~a[~a]~%" (frac 1 "h")
      (format nil "~{~a~^+~}" (mapcar #'concat fractions dltas)))
    (format t "~a[~a]~%" (frac 1 (nde-h *target-nde*))
            (apply #'concat (mapcar #'concat dltas-in-nums fractions)))))

(finite-subs #'target-f-src (nde-i *TARGET-NDE* 4) 1 *TARGET-NDE*)

