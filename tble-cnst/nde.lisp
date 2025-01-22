(in-package #:num-methods.tble-cnst)

(defstruct (nde-t
            (:CONC-NAME nde-))
  beg end n)

(defun nde-h (nde)
  (/ (- (nde-end nde) (nde-beg nde))
    (nde-n nde)))

(defun nde-i (nde i)
  (+ (nde-beg nde) (* i (nde-h nde))))

(defun insert_from_range (msg nde &key (stream t))
  (insert_dialog msg #'(lambda (x)
                         (and (realp x)
                              (<= (nde-beg nde) x) (<= x (nde-end nde))))
                 :stream stream))
