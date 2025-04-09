(defun f (x)
  (* 27
     (sqrt x)
     (cos (+ 8
             (sqrt (* x x x))))))

(loop for i from 0 to 5 collect
      (+ 3.35
         (* 0.002 i)))
