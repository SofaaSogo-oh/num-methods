(in-package #:num-methods.tble-cnst)

(defun insert_dialog (msg cnd &key (stream t))
  (labels ((insert_dialog_ () 
             (format stream msg)
             (let ((x (read)))
               (if (funcall cnd x) x
                   (insert_dialog_)))))
    (insert_dialog_)))


(defun insert_natural (msg &key (stream t))
  (insert_dialog msg #'(lambda (x)
                         (and (integerp x)
                              (>= x 0)))
                 :stream stream))

