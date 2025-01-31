(in-package #:num-methods.lab3)

(defparameter *legendre-poly-root-4*
  '(-0.86113631d0 -0.33998104d0 0.33998104d0 0.86113631d0))

(defparameter *legendre-poly-root-8*
  '(-0.96028986d0 -0.79666648d0 -0.52553242d0 -0.18343464d0
    0.18343464d0 0.52553242d0 0.79666648d0 0.96028986d0))

(defparameter *gauss-system-root-4*
  '(0.34785484d0 0.65214516d0 0.65214516d0 0.34785484d0))

(defparameter *gauss-system-root-8*
  '(0.10122854d0 0.22238104d0 0.31370664d0 0.36268378d0
    0.36268378d0 0.31370664d0 0.22238104d0 0.10122854d0))

(defun gauss (f nde g-rts l-rts)
  (let* ((a (nde-beg nde))
         (b (nde-end nde))
         (dlta (/ (- b a) 2)))
    (labels ((x_i (t_i) (+ a dlta (* dlta t_i)))
             (f_i (t_i) (funcall f (x_i t_i))))
      (reduce #'+
              (mapcar #'(lambda (a_i t_i) (* a_i (f_i t_i) dlta))
                      g-rts l-rts)))))
