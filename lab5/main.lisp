(in-package #:num-methods.lab5)

(defun target-y (x)
  (exp (sin x)))

(defun dy/dx (x y)
  (* y
     (cos x)))

(defun target-nde (n)
  (make-nde-t :beg 0.d0
              :end 1.d0
              :n n))

(defparameter *part10* (target-nde 10))
(defparameter *part20* (target-nde 20))

(defparameter *y_0* 1)

(defstruct clc-res
  res
  res-x
  clc)

(defun euler (f nde y_0)
  (let ((n (nde-n nde))
        (h (nde-h nde)))
    (labels ((lp (k y_k)
               (if (> k n) (reverse y_k)
                   (let* ((k1 (1+ k))
                          (xk (nde-i nde k))
                          (yk (car y_k))
                          (yk1 (+ yk (* h (funcall f xk yk))))
                          (y_k1 (cons yk1 y_k)))
                     (lp k1 y_k1)))))
      (lp 0 (list y_0)))))

(defun rk4-iteration (f h x y)
  (let* ((h2 (/ h 2))
         (k-1k (* h (funcall f x y)))
         (k-2k (* h (funcall f (+ x h2) (+ y (/ k-1k 2)))))
         (k-3k (* h (funcall f (+ x h2) (+ y (/ k-2k 2)))))
         (k-4k (* h (funcall f (+ x h)  (+ y k-3k)))))
    (+ y
       (/ (+ k-1k (* 2 k-2k) (* 2 k-3k) k-4k)
          6))))

(defun rk4 (f nde y_0)
  (let* ((n (nde-n nde))
         (h (nde-h nde)))
    (labels ((f (x y) (funcall f x y))
             (lp (k y_k)
               (if (> k n) (reverse y_k)
                   (let* ((k1 (1+ k))
                          (xk (nde-i nde k))
                          (yk (car y_k))
                          (yk1 (rk4-iteration f h xk yk))
                          (y_k1 (cons yk1 y_k)))
                     (lp k1 y_k1)))))
      (lp 0 (list y_0)))))

(defparameter *rk-tmp-h-hh* 
  '("xₖ" "yₖ" "yₕ" "yₘ" "Δyₕ" "Δyₖ" "h"))

(defun rkt4 (f nde y0 eps &key (display t) (trg-y #'target-y))
  (let* ((h0 (nde-h nde))
         (a (nde-beg nde))
         (b (nde-end nde))
         (header (format nil "Таблица значений при заданном ε=~a" eps))
         (table (ascii-table:make-table *RK-TMP-H-HH* :header header)))
    (labels ((f (x y) (funcall f x y))
             (disp (&rest args)
               (ascii-table:add-row table args))
             (lp (x y h clc y_k x_k)
               (if (> x b) (make-clc-res :res (reverse y_k)
                                         :clc clc
                                         :res-x (reverse x_k))
                   (let* ((m (* 2 h))
                          (xh (+ x h))
                          (xhh (+ xh h))
                          (yh (rk4-iteration f h x y))
                          (yhh (rk4-iteration f h xh yh))
                          (ym (rk4-iteration f m x y))
                          (dym (abs (- yhh ym))))
                     (let* ((y (funcall trg-y xh))
                            (dy (abs (- y yh))))
                       (disp xh y yh nil nil dy h))
                     (let* ((y (funcall trg-y xhh))
                            (dy (abs (- y yhh))))
                       (disp xhh (funcall trg-y xhh) yhh ym dym dy h))
                     (if (< dym eps)
                         (lp xhh yhh h clc (cons yh y_k) (cons xh x_k))
                         (lp x y (/ h 2) (1+ clc) y_k x_k))))))
      (when display (disp a (funcall trg-y a)(funcall trg-y a)  nil nil 0 h0))
      (let ((res (lp a y0 h0 0 nil nil)))
        (when display (ascii-table:display table))
        res))))

  ;'("xₖ" "yₖ" "yₕ" "yₘ" "Δyₕ" "Δyₖ"))
(progn (rkt4 #'dy/dx *part10* 1 1d-7) nil)

(defun calc-eps (f nde y0 &key (os t))
  (labels ((bp (eps?)
             (let* ((clc (clc-res-clc (rkt4 f nde y0 eps? :display nil))))
               (format os "~a~%" (list eps? clc))
               (cond ((= clc 1) eps?)
                     ((< clc 1) (bp (* (/ 1 2) eps?)))
                     ((> clc 1) (bp (* (/ 3 2) eps?)))))))
    (bp (nde-h nde))))

(calc-eps #'dy/dx *PART10* 1)

(defun disp-based-nde-1-step-rk4 (f nde y0)
  (let ((eps (calc-eps f nde y0 :os nil)))
    (rkt4 f nde y0 eps)
    nil))

(disp-based-nde-1-step-rk4 #'dy/dx *PART10* 1)
(disp-based-nde-1-step-rk4 #'dy/dx *PART20* 1)

(defun adams (f nde y_0)
  (let* ((n (nde-n nde))
         (h (nde-h nde))
         (mults '(55 -59 37 -9))
         (y_0 (reverse (subseq (rk4 f nde y_0) 0 4))))
    (labels ((f (x y) (funcall f x y))
             (x-i (i i-) (nde-i nde (- i i-)))
             (lp (k y_k)
               (if (> k n) (reverse y_k)
                   (let* ((k1 (1+ k))
                          (yk (car y_k))
                          (xik (mapcar (curry #'x-i k) (iota 4)))
                          (yik (subseq y_k 0 4))
                          (yi-k (mapcar #'f xik yik))
                          (yk1 (+ yk
                                  (* (/ h 24)
                                   (reduce #'+ (mapcar #'* yi-k mults)))))
                          (y_k1 (cons yk1 y_k)))
                     (lp k1 y_k1)))))
      (lp 3 y_0))))


(defun adams-iter (f lst-xlist lst-ylist h)
  (let* ((xik (subseq lst-xlist 0 4))
         (yik (subseq lst-ylist 0 4))
         (mults '(55 -59 37 -9))
         (yi-k (mapcar f xik yik))
         (yk (car yik))
         (yk1 (+ yk
                 (* (/ h 24)
                    (reduce #'+ (mapcar #'* yi-k mults))))))
    yk1))

(defun adams-tmp (f nde y0 eps)
  (let* ((res (rkt4 f nde y0 eps :display nil))
         (x-0 (reverse (subseq (clc-res-res-x res) 0 4)))
         (h0 (- (car x-0) (cadr x-0)))
         (y-0 (reverse (subseq (clc-res-res res) 0 4))))
    h0))

(adams-tmp #'dy/dx *PART10* 1 1d-7)

(euler #'dy/dx *PART10* 1.d0)

(defparameter *table-hh*
  '("i" "xᵢ" "y(xᵢ)"
    "yᵉᵢ" "Δyᵉᵢ"
    "yᵖᵢ" "Δyᵖᵢ"
    "yᴬᵢ" "Δyᴬᵢ"))


(defun table-for-partion(part y_0)
    (let* ((n (nde-n part))
           (header (format nil "Решение задачи при n = ~a" n))
           (table (ascii-table:make-table *table-hh* :header header))
           (add-row (curry #'ascii-table:add-row table))
           (ii (iota (1+ (nde-n part))))
           (x_i (mapcar (curry #'nde-i part) ii))
           (y_i (mapcar #'target-y x_i))
           (ye_i (euler #'dy/dx part y_0))
           (dye_i (mapcar #'abs (mapcar #'- ye_i y_i)))
           (yp_i (rk4 #'dy/dx part y_0))
           (dyp_i (mapcar #'abs (mapcar #'- yp_i y_i)))
           (ya_i (adams #'dy/dx part y_0))
           (dya_i (mapcar #'abs (mapcar #'- ya_i y_i)))
           (res-table (apply #'mapcar #'list (list ii x_i y_i ye_i dye_i yp_i dyp_i ya_i dya_i))))
      (mapcar add-row res-table)
      (ascii-table:display table)))

(defun main nil
  (table-for-partion *PART10* *Y_0*)
  (table-for-partion *PART20* *Y_0*)
  (disp-based-nde-1-step-rk4 #'dy/dx *PART10* 1)
  (disp-based-nde-1-step-rk4 #'dy/dx *PART20* 1))
  

(main)
