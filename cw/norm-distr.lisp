(in-package #:num-methods.cw)

(defun normalized-random (&optional (state *RANDOM-STATE*))
  (let ((u (random 1.0 state))
        (v (random 1.0 state)))
    (*
      (sin (* 2 pi u))
      (sqrt (* -2 (log v))))))

(defun normal-random (&optional (mean-v 0) (disp 1) (state *RANDOM-STATE*))
  (+ mean-v
     (* disp
        (normalized-random state))))

