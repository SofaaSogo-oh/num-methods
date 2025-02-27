(in-package :num-methods.lab0)

(defun target-f (x)
  (+ (exp (* 2 x))
     (* x x x)))

(defun target-df (x)
  (+ (* 2 (exp (* 2 x)))
     (* 3 x x)))

(defun argument_check (x)
  (unless (and (<= -1 x) (<= x 5))
    (error "Given argument should be in [-1, -1/2]")))

(defparameter *header*
  '("k" "xₖ" "xₖ₋₁" "Δx" "Δy"))

(defun meta_find_root (f nxt init_aprx eps &key (title nil))
  (let ((table (ascii-table:make-table *header* :header title)))
    (labels ((loop_ (k aprx)
               (let* ((k1 (1+ k))
                      (aprx1 (funcall nxt aprx))
                      (dx (abs (- aprx aprx1)))
                      (dy (abs (funcall f aprx1))))
                 (ascii-table:add-row table (list k1 aprx1 aprx dx dy))
                 (if (and (< dx eps) (< dy eps))
                     (list k aprx1)
                     (loop_ k1 aprx1)))))
      (argument_check init_aprx)
      (let ((res (loop_ 0 init_aprx)))
        (ascii-table:display table)
        (cons init_aprx res)))))

(defun sim (init_aprx eps)
  (labels ((f (x) (target-f x))
           (nxt (x) (- x (/ (f x) 2))))
    (meta_find_root #'f #'nxt init_aprx eps
                    :title "Метод простых итераций")))

(defun newthon (init_aprx eps)
  (labels ((f (x) (target-f x))
           (g (x) (target-df x))
           (nxt (x) (- x (/ (f x)
                            (g x)))))
    (meta_find_root #'f #'nxt init_aprx eps
                    :title "Метод Ньютона")))

(defun mod_newthon (init_aprx eps)
  (labels ((f (x) (target-f x))
           (g (x) (target-df x))
           (nxt (x) 
             (let ((g_0 (g init_aprx)))
               (- x (/ (f x) g_0)))))
    (meta_find_root #'f #'nxt init_aprx eps
                    :title "Модифицированный метод Ньютона")))

(defparameter *res-table-hh*
  '("x_0" "k" "eps"))

(defun main () 
  (let ((num-of-inits (insert_natural "Введите число начальных приближений"))
        (points-range (make-nde-t 
                       :beg -1.d0
                       :end -0.5d0))
        (epss '(1.d-3 1.d-5))
        (methods (list #'sim #'newthon #'mod_newthon))
        (table (ascii-table:make-table *res-table-hh* :header "Результирующая таблица")))
    (dotimes (i um-of-inits)
      (let ((x_0i (insert_from_range 
                    (format nil "x_~a" i)
                    points-range)))
        (format t "~A~%"(mapcar #'(lambda (method)
                                   (mapcar (curry method x_0i) epss))
                              methods))))))
