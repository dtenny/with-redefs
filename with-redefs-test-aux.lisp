(in-package :cl-user)

(defpackage :with-redefs-test-aux
  (:use :cl)
  ;; Note selective exporting
  (:export #:fn-a #:call-fn-a
           #:fn-b #:*results*)
  (:documentation "An extra test package for functions in a separate compilation unit."))

(in-package :with-redefs-test-aux)

(defparameter *results* nil "A place for functions to side-effect for testing.")

;; Some functions to redef
(defun fn-a () (push 'fn-a *results*))
(defun fn-b (x) (push (list 'fn-b x) *results*))
(defun fn-c (x y) (push (list 'fn-c x y) *results*))
(defun call-fn-a () (funcall 'fn-a))
(defun call-fn-b (x) (funcall 'fn-b x))
(defun call-fn-c (x y) (funcall 'fn-c x y))

