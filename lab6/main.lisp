(in-package #:num-methods.lab6)

(defparameter *x-split1* 
  (make-nde-t :beg 1 
              :end 2
              :n 10))

(defparameter *x-split2* 
  (make-nde-t :beg 1 
              :end 2
              :n 20))

(setf (symbol-function 'p)
      #'cos)

(defun f (x)
  (/ 1 x))

(defun q (x)
  -1)

(defun m_i (nde i)
  (let* ((h (nde-h nde))
         (x_i (nde-i nde i))
         (q_i (q x_i))
         (p_i (p x_i)))
    (/ (- (* 2 h h q_i) 4)
       (+ 2 (* h p_i)))))

(defun r_i (nde i)
  (let* ((h (nde-h nde))
         (x_i (nde-i nde i))
         (p_i (p x_i)))
    (/ (- 2 (* h p_i))
       (+ 2 (* h p_i)))))

(defun phi_i (nde i)
  (let* ((h (nde-h nde))
         (x_i (nde-i nde i))
         (f_i (f x_i))
         (p_i (p x_i)))
    (/ (* 2 h h f_i)
       (+ 2 (* h p_i)))))

(defun direct-cd (alp0 alp1 nde)
  (let* ((h (nde-h nde))
         (c0 (/ alp0
              (- (* h alp1) alp0)))
         (d0 (/ (* 2 h) alp0))
         (n-1 (1- (nde-n nde))))
    (labels ((iter (i ci-1 di-1 res)
               (if (> i n-1) res
                   (let* ((mi (m_i nde i))
                          (ri (r_i nde i))
                          (phii (phi_i nde i))
                          (ci (/ 1 (- mi (* ri ci-1))))
                          (di (- phii (* ri ci-1 di-1)))
                          (res (cons (list ci di) res)))
                     (iter (1+ i) ci di res)))))
      (iter 1 c0 d0 (list (list c0 d0))))))

(setf (symbol-function 'target-cd)
      (curry #'direct-cd 1 2))

(defun reverse-y (cd B bet0 bet1 nde)
  (let* ((h (nde-h nde))
         (n-1 (1- (nde-n nde)))
         (cd (funcall cd nde))
         (cn-1 (first (car cd)))
         (dn-1 (second (car cd)))
         (yn (/ (+ (* B h) (* bet0 cn-1 dn-1))
                (+ (* bet0 (+ 1 cn-1)) (* h bet1)))))
    (labels ((iter (i cd yi+1 res)
               (if (< i 0) res
                 (let* ((ci (first (car cd)))
                        (di (second (car cd)))
                        (yi (* ci (- di yi+1)))
                        (res (cons yi res)))
                   (iter (1- i) (cdr cd) yi res)))))
      (iter n-1 cd yn (list yn)))))

(setf (symbol-function 'target-y)
      (curry #'reverse-y 
             #'target-cd 0 0 1))

(target-y *X-SPLIT1*)
(target-y *X-SPLIT2*)

(defun main (nde)
  (let* ((n (nde-n nde))
         (table (ascii-table:make-table 
                  (mapcar 
                     (curry #'format nil "~aᵢ")
                     '("x" "y" "m" "r" "φ" "c" "d"))
                  :header (format nil "n = ~a" n)))
         (i (loop for i from 0 to n collect i))
         (xi (mapcar (curry #'nde-i nde) i))
         (yi (target-y nde))
         (mi (mapcar (curry #'m_i nde) i))
         (ri (mapcar (curry #'r_i nde) i))
         (phii (mapcar (curry #'phi_i nde) i))
         (cd (reverse (target-cd nde)))
         (ci (mapcar #'first cd))
         (di (mapcar #'second cd)))
    (mapcar (compose
             (curry #'ascii-table:add-row table)
             #'list)
            xi yi mi ri phii ci di) 
    (ascii-table:display table)))

(main *x-split1*)
(main *X-SPLIT2*)
