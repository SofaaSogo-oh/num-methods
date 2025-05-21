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

(defun display-sigma-points (alist f &key (stream t))
  (when stream
    (format stream "Points total: ~a~%" (length alist))
    (format stream "~{~a~%~}"
            (mapcar (lambda (x)
                      (format nil "~a -> ~a"
                        x (apply f x)))
                    alist)))) 

(defun special-print (alist indicator f &key (stream nil))
  (progn
    (format stream "~{~a~%~}"
            (mapcar (lambda (x)
                      (if (null (apply indicator x))
                          (format nil "~a \\not\\in D \\\\" x)
                          (format nil "~a \\to ~a \\\\" x (apply f x))))
                    alist))
    (let* ((n (length alist))
           (sublist 
            (remove-if
             #'null
              (mapcar (lambda (x) 
                        (if (null (apply indicator x))
                            nil
                            (apply f x))) alist)))
           (res (/ (reduce #'+ sublist) n))
           (n_ (length sublist)))
      (format stream "I = \\frac{1}{~a}\\sum_{k=0}^{~a} =\\frac{1}{~a}(~a) = ~a~%"
              n n_ n
              (format nil "~{~a~^+~}" sublist)
              res))))

(special-print (get-freq-mrx 20 2) #'sigma-indicator #'reduced-changed-int :stream t)

(defun calc-integral-body (n points f &key (stream nil))
  (display-sigma-points points f :stream stream)
  (/ (reduce #'+
             (mapcar (curry #'apply f) points))
     n))

(defun calc-integral (n indicator splits f &key (stream nil))
  (let ((points (get-sigma-points n indicator splits)))
    (calc-integral-body n points f :stream stream)))

(defun calc2-integral (n indicator splits f &key (stream nil) (maxv 1024))
  (let* 
      ((points (get-freq-mrx n (1+ (length splits))))
       (flterd (remove-if-not 
                (lambda (x) 
                  (let ((f-x (car x))
                        (v-x (cdr x)))
                    (and (apply indicator v-x)
                      (< f-x (/ (apply f v-x) maxv)))))
                points)))
    (format stream "~{~a~%~}" 
            (mapcar
              (lambda (x)
                (format nil "~a ~a ~a" 
                        (car x)
                        (cdr x)
                        (/ (apply f (cdr x)) maxv)))
              flterd))
    (* maxv (/ (length flterd) n))))

(float (calc2-integral 20 #'sigma-indicator (list *X-SPLIT* *Y-SPLIT*) #'reduced-changed-int :stream t))

(float (calc-integral 20 #'sigma-indicator (list *X-SPLIT* *Y-SPLIT*) #'reduced-changed-int :stream t))
       

(defun calc-1 (points)
 (calc-integral points #'sigma-indicator 
                      (list *X-SPLIT* *Y-SPLIT*)
                      #'reduced-changed-int))

(defun calc-2 (points)
  (calc2-integral points #'sigma-indicator (list *X-SPLIT* *Y-SPLIT*) #'reduced-changed-int))

(defun main (samples points &key (func #'calc-1))
  (loop for i from 1 to samples collect 
        (funcall func points)))

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
  

(display-list (collect-frequency (main 100 60) 8))

(calc-integral 20 #'sigma-indicator (list *X-SPLIT* *Y-SPLIT*) #'reduced-changed-int :stream t)


(main 20 30)

(defun calc-mean (alist)
  (/ (reduce #'+ alist) 
     (length alist)))

(defun calc-disp (alist) 
  (let ((mean (calc-mean alist)))
    (/ (reduce 
         #'+ 
         (mapcar 
          (lambda (x) 
            (* (- x mean) (- x mean)))
          alist))
       (1- (length alist)))))

(defun calc-normal-d (alist)
  (sqrt (calc-disp alist)))

(defun alist-info (alist &key (stream t))
  (format stream "$m_x$ & $D_x$ & $\\sigma_x$ & $|m_x - I|$ \\\\~%")
  (format stream "$~a$ & $~a$ & $~a$ & $~a$ \\\\~%"
          (float (calc-mean alist))
          (float (calc-disp alist))
          (calc-normal-d alist)
          (float (abs (- *TARGET-INT-VALUE* (calc-mean alist))))))
  

(defun alist-normal-d (ssamples samples points &key (func #'calc-1))
  (loop for i from 0 to ssamples collect (main samples points :func func)))

(main 100 20 :func #'calc-2)

(alist-info (main 100 20))
(alist-info (main 100 50))
(alist-info (main 100 100))
(alist-info (main 100 200))
(alist-info (main 100 300))

(alist-info (main 100 20 :func #'calc-2))
(alist-info (main 100 50 :func #'calc-2))
(alist-info (main 100 100 :func #'alc-2))
(alist-info (main 100 200 :func #'calc-2))
(alist-info (main 100 300 :func #'calc-2))

(defun print-table (func)
  (let ((table (ascii-table:make-table '("N" "mₓ" "dₓ"))))
    (mapcar 
      (lambda (n) 
        (ascii-table:add-row table 
         (let* ((normal-d (mapcar #'calc-normal-d (alist-normal-d 30 100 n :func func)))
                (mean-normal-d (calc-mean normal-d))
                (normal-normal-d (calc-normal-d normal-d)))
             (list 
               n 
               mean-normal-d 
               normal-normal-d))))
      '(20 50 100 200 300 400 500))
    (ascii-table:display table)
    nil))

(print-table #'calc-1)
(print-table #'calc-2)

