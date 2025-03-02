(in-package #:num-methods.cw)

(defun display-list (alist) 
  (progn
    (mapcar #'(lambda (x) 
                (format t "~v@{~a~:*~}~%" x #\*))
            alist)
    alist))

(defun collect-frequency (alist k)
  (let* ((min-val (reduce #'min alist))
         (max-val (reduce #'max alist))
         (h (/ (- max-val min-val)
               k))
         (res-freq (make-array k :initial-element 0)))
    (loop for x in alist do
          (let* ((inx (floor (/ (- x min-val)
                               h)))
                 (cti (min (1- k) inx)))
            (incf (aref res-freq cti))))
    (coerce res-freq 'list)))

