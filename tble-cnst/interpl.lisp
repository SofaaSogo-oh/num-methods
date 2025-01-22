(in-package #:num-methods.tble-cnst)

(defparameter *interpl-table* 
  '("x" "f(x)" 
    "Pᴵₙ(x)" "ΔPᴵₙ(x)"
    "P𝕀ₙ(x)" "ΔP𝕀ₙ(x)"
    "Lₙ(x)" "ΔLₙ(x)"))

(defparameter *interpl-table-deriv*
  (let ((ssq (subseq *INTERPL-TABLE* 0 6)))
    (mapcar #'(lambda (elm inx)
                (if (= inx 1) "f'(x)"
                  elm))
            ssq (iota (length ssq)))))

(defun finite-subs (f x n bnd) 
  (let ((h (nde-h bnd)))
    (labels ((finite-subs_ (k z acc)
               (if (> k n) acc
                 (finite-subs_ 
                   (1+ k)
                   (* -1 z)
                   (+ acc 
                      (* z
                         (combinations_cnt n k)
                         (funcall f (+ x (* k h)))))))))
      (finite-subs_ 0 
                    (if (zerop (mod n 2)) 1 -1) 
                    0))))


