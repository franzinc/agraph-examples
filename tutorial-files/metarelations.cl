
(in-package :triple-store-user)

(defvar *relations* (make-hash-table))

(defun clear-relations ()
  (setf *relations* (make-hash-table)))  

(defun register-relations (&rest list)
  (dolist (e list)
    (when (not (gethash e *relations*))
      (let ((form `(<- (relation ?x ?y ,e)
                       (,e ?x ?y))))
        (when (= 0 (hash-table-count *relations*))
          (setf (car form) '<--))
        (setf (gethash e *relations*) t)
        (eval form)
        (pprint form)))))
