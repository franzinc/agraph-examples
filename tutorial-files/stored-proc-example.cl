;; sample stored procedure

(defpackage :my-package
   (:use :excl :common-lisp :db.agraph))

(in-package :my-package)

(define-condition my-stored-proc-error (simple-error) ())

(defun my-stored-proc-error (fmt &rest args)
  (error (make-condition 'my-stored-proc-error
                         :format-control fmt
                         :format-arguments args)))

;; agraph 3.x style:
;;    (def-kbi-call "addTwo"  dbs-add-two-numbers)
;;
;;
;; (defun dbs-add-two-numbers (arg-vec)
;;    ;; takes two values and adds them
;;   (if* (not (eql 2 (length arg-vec)))
;;     then "wrong number of args"
;;     else (write-to-string
;;       (+ (or (parse-integer (aref arg-vec 0) :junk-allowed t) 0)
;;          (or (parse-integer (aref arg-vec 1) :junk-allowed t) 0)))))
;;

; ag 4.x style where the whole argument vector is given
; to the stored procedure and it pulls out the values
;
(def-stored-proc addTwo (&whole arg-vec)
   ;; takes two values and adds them
   (db.agraph::log-info :addtwo "arg vec is ~s" arg-vec)
   (if* (not (eql 2 (length arg-vec)))
      then (my-stored-proc-error "wrong number of args")
      else (write-to-string
       (+ (or (parse-integer (aref arg-vec 0) :junk-allowed t) 0)
          (or (parse-integer (aref arg-vec 1) :junk-allowed t) 0)))))


; ag 4.x style where we let the def-stored-proc code generate code
; to check if the correct number of arguments were passed in the
; argument vector and if not to return an error indication

(def-stored-proc addTwoVersion2 (a b)
   ;; takes two values and adds them
   (write-to-string
    (+ (or (parse-integer a :junk-allowed t) 0)
       (or (parse-integer b :junk-allowed t) 0))))
