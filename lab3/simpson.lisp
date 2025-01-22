(in-package #:num-methods.lab3)

(defun simpson (f nde)
  (let* ((h (nde-h nde))
         (n (nde-n nde)))
    (* (/ h 3)
       (+ (funcall f (nde-beg nde))
          (funcall f (nde-end nde))
          (reduce
            #'+
            (mapcar
              #'(lambda (i)
                  (* 2 (1+ (mod i 2))
                    (funcall (compose f (curry #'nde-i nde)) i)))
              (iota (1- n) :start 1)))))))

;(defun simpson (f nde)
;  (let* ((h (nde-h nde))
;         (n (nde-n nde))
;         (f_i (compose f (curry #'nde-i nde))))
;    (* (/ h 3)
;       (reduce 
;         #'+
;         (mapcar
;           #'(lambda (i)
;               (+
;                (funcall f_i (1- i))
;                (* 4 (funcall f_i i))
;                (funcall f_i (1+ i))))
;           (loop for i from 1 to (1- n) by 2 collect i))))))
