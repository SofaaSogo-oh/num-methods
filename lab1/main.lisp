(in-package #:num-methods.lab1)


(defparameter *target-nde* (make-nde-t
                             :beg 3.35d0
                             :end 3.36d0
                             :n 5))

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

(defun finite-subs-tree (nde f)
  (let* ((n (nde-n nde))
         (h (nde-h nde))
         (a (nde-beg nde)))
    (labels ((calc-fs (k m)
               (finite-subs f (+ a (* h k)) m nde))
             (fsst_ (k m)
               (let ((k1 (1+ k))
                     (m1 (1- m)))
                 (if (zerop m) (list (calc-fs k m) nil)
                     (list (calc-fs k m)
                           (list (fsst_ k m1)
                                 (fsst_ k1 m1)))))))
      (fsst_ 0 n))))

(defun disp-finite-subs-tree (nde f)
  (let* ((n (nde-n nde))
         (h (nde-h nde))
         (a (nde-beg nde))
         (memo (make-hash-table :test #'equal))
         (layers (make-array (1+ (* n 2)) :initial-element nil)))
    (labels ((calc-fs (k m)
               (finite-subs f (+ a (* h k)) m nde))
             (add-to-layer (i k m)
               (push (list k m) (aref layers i)))
             (dfsst_ (i k m)
               (let ((cached (gethash (list k m) memo))
                     (k1 (1+ k)) (m1 (1- m))
                     (i+ (1+ i)) (i- (1- i)))
                 (unless (or (< m 0) cached)
                   (setf (gethash (list k m) memo) t)
                   (dfsst_ i+ k1 m1)
                   (add-to-layer i k m)
                   (dfsst_ i- k m1)))))
      (dfsst_ n 0 n)
      (loop for i from (* 2 n) downto 0 do
            (format t "~V@T~{~a~8@T~}~%" (* 8 (mod i 2)) 
                    (mapcar #'(lambda (curr) 
                                (let* ((k (car curr))
                                       (m (cadr curr))
                                       (clc (calc-fs k m)))
                                  (cond ((= m 0) (format nil "y_~a = ~a" k clc))
                                        ((= m 1) (format nil "Δy_~a = ~a" k clc))
                                        (t (format nil "Δ~ay_~a = ~a" m k clc)))))
                            (reverse (aref layers i))))))))

(disp-finite-subs-tree *TARGET-NDE* #'target-f)

(finite-subs-tree *TARGET-NDE* #'target-f)

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

(defun format-sci-to-tex (num)
  (let* ((str-repr (format nil "~e" num))
         (parts (split "d" str-repr))
         (mantissa (first parts))
         (exponent (second parts)))
    (format nil "~a \\cdot 10^{~a}" mantissa exponent)))

(defun lagrange-c-i-tex (nde f i)
  (format nil "c_~a = \\frac{y_~a}{~a} = ~a"
    i
    i
    (format nil "~{~a~}"
      (loop for j from 0 to (nde-n nde) collect
        (if (= i j) ""
          (format nil "(x_~a-x_~a)" i j))))
    (format-sci-to-tex (lagrange.c_i nde f i))))

(defun lagrange-c-i-tex-res (nde f i)
  (format nil "~a ~a"
    (format-sci-to-tex (lagrange.c_i nde f i))
    (format nil "~{(x-~a)~}" 
      (mapcar (curry #'nde-i nde)
        (iota (1+ (nde-n nde)))))))

(defun disp-lagrange-c-tex (nde f)
  (format t "~{~a~%~}"
    (let ((str-print (curry #'lagrange-c-i-tex nde f)))
      (mapcar str-print (iota (1+ (nde-n nde)))))))

(defun lab1-demo nil
  (disp-finite-subs-tree *TARGET-NDE* #'target-f)
  (disp-lagrange-c-tex *TARGET-NDE* #'target-f)
  (main))
  
(lab1-demo)

