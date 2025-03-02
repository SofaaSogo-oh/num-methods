(in-package #:num-methods.cw)

(display-list 
 (mapcar (compose #'floor (rcurry #'/ 200))
  (collect-frequency (loop for i from 0 to 10000 collect
                        (normalized-random)) 10)))

(defun test-t-value nil
  (loop for k from 1 to 8 do
        (format t "~{~18a~^ ~}~%" (cons k (mapcar
                                           (curry #'t-distribution k)
                                           (mapcar (compose (curry #'- 1)
                                                            (rcurry #'/ 2))
                                                   (list 0.10 0.05 0.02 0.001 0.002 0.001)))))))


(defun get-freq-mrx (n m)
  (let ((state (make-random-state t)))
    (loop for _ from 1 to n collect
          (loop for _ from 1 to m collect
                (random 1.0 state)))))


(defun D-area-indicator (x y)
  (and (< x 6)
       (< y (* 2 x))
       (> (* x y) 8)))

(defun sigma-indicator (x y)
  (and (< x 1)
       (< (- (* 4 y)
             (* 3 x))
          1)
       (> (* 
            (+ 2 (* 4 x))
            (+ 4 (* 32 y)))
          24)))

(defun origin-int (x y)
  (+ (* 2 x)
     y))

(defparameter *x-split*
  (make-nde-t :beg 2
              :end 6))

(defparameter *y-split*
  (make-nde-t :beg (/ 4 3)
              :end 12))

(defun map-to-old (sp x)
  (+ (nde-beg sp) (* x (nde-rng sp))))

(defun changed-int (xi ni)
  (*
   (nde-rng *x-split*) 
   (nde-rng *y-split*)
   (origin-int
     (map-to-old *X-SPLIT* xi) 
     (map-to-old *Y-SPLIT* ni))))

(defun reduced-changed-int (xi ni)
  (* (/ 128 9)
     (+ (* 24 xi)
        (* 32 ni)
        16)))

(defparameter *target-int-value* 
  (/ 1024
     3))

(defun get-area-points (n indicator splits)
  (remove-if-not
    (curry #'apply
     indicator)
    (mapcar 
     (curry
      #'mapcar #'map-to-old
      splits)
     (get-freq-mrx n (length splits)))))

(defun get-sigma-points (n indicator splits)
  (remove-if-not
    (curry #'apply
           indicator)
    (get-freq-mrx n (length splits))))

(length (get-sigma-points 1000 #'sigma-indicator (list *X-SPLIT* *Y-SPLIT*)))

(defun calc-integral (n indicator splits f)
  (let ((points (get-sigma-points n indicator splits)))
    (/ (reduce #'+
               (mapcar (curry #'apply f) points)) 
       n)))

(defun main nil
  (loop for i from 1 to 30 collect 
        (calc-integral 3000000 #'sigma-indicator 
                      (list *X-SPLIT* *Y-SPLIT*)
                      #'reduced-changed-int)))

(defun calc-confidence-mean (alist eps)
  (let* ((n (length alist))
         (mean-v (/ (reduce #'+ alist)
                    n))
         (disp-v (/ (reduce #'+
                      (mapcar
                         #'(lambda (x) (* (- x mean-v)
                                          (- x mean-v)))
                       alist))
                    (1- n)))
         (teps-v (t-distribution (1- n) (- 1 (/ eps 2))))
         (dlta (/ (* teps-v disp-v)
                  (sqrt n))))
    (list (- mean-v dlta) (+ mean-v dlta))))
  

;(calc-confidence-mean (main) 0.05)
