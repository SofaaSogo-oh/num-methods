(in-package #:num-methods.lab4)

(defun target-f (x y)
  (- (* 5 x)
     (* 2 y)))

(defun simpson-mrx (n m i j)
  (labels ((m2 (x) (* 2 (1+ (mod x 2))))
           (b2 (x b) (if (and (< 0 x) (< x b)) 1 2)))
    (/ (* (m2 i) (m2 j))
       (* (b2 i n) (b2 j m)))))

(defun target-D (x y)
  (if (and 
        (>= y 0)
        (<= y x)
        (<= y (- 4 x)))
      1 0))

(defparameter *target-val* 
  (/ 104 3))

(defun make-nde-x (n)
  (make-nde-t :beg 0 :end 4 :n n))

(defun make-nde-y (m)
  (make-nde-t :beg 0 :end 2 :n m))

(defun print-simpson-mrx(n m) 
  (loop for i from 0 to n do
        (format t "~{~4D~}~%"
          (loop for j from 0 to m collect
                (simpson-mrx n m i j)))))

(defun simpson (f D-ind nde-D-n nde-D-m)
  (let ((n (nde-n nde-D-n))
        (m (nde-n nde-D-m))
        (h (nde-h nde-D-n))
        (k (nde-h nde-D-m))
        (x-i (curry #'nde-i nde-D-n))
        (y-j (curry #'nde-i nde-D-m)))
    (* (/ (* h k)
          9)
       (reduce #'+
               (loop for i from 0 to n 
                     append (loop for j from 0 to m
                                  collect (* (simpson-mrx n m i j)
                                             (funcall f (funcall x-i i) (funcall y-j j))
                                             (funcall D-ind (funcall x-i i) (funcall y-j j)))))))))

(defparameter *iint-header* '("n" "m" "I" "Iₛ" "ΔIₛ"))

(defun add-row-iint (table target-n target-m)
  (let* ((n (nde-n target-n))
         (m (nde-n target-m))
         (I *TARGET-VAL*)
         (I_s (simpson #'target-f #'target-d target-n target-m))
         (dI_s (abs (- I I_s))))
    (ascii-table:add-row table (list n m (* I 1.d0) (* I_s 1.d0) (* dI_s 1.d0)))))

(defun main ()
  (let ((table (ascii-table:make-table *iint-table* :header "Кратный интеграл"))
        (total_nm (insert_natural "Введите число вариаций разбиений: ")))
    (dotimes (i total_nm)
      (let ((n (* 2 (insert_natural (format nil "n_~A: " i))))
            (m (* 2 (insert_natural (format nil "m_~A: " i)))))
        (add-row-iint table (make-nde-x n) (make-nde-y m))
        (print-simpson-mrx m n)))
    (ascii-table:display table)))
