(in-package :cl-user)

(defpackage :with-redefs-test
  (:use :cl :with-redefs :with-redefs-test-aux :fiveam)
  (:export #:run-tests)
  (:documentation "Tests for the :with-redefs package."))

(in-package :with-redefs-test)

;;;; NOTE: The expected operation of these tests may depend on compiler optimization
;;;; declarations.  Just be aware of it.  None of the with-redefs materals make any declarations.

(def-suite test-suite :description ":with-redefs tests")
(in-suite test-suite)

;;;
;;; Some of these tests may serve no purpose.  I was extra paranoid because ECL, and
;;; ECL only, on some tests I ran with a prior stab at with-redefs on 6 lisps,
;;; wouldn't work properly on functions that were wrapped in a separate compilation
;;; unit. I never quite got to the bottom of it. It's hard to write tests for
;;; failures you don't know to expect, but that's what I'm trying to do here.
;;;

(test basic-redefining-WITHOUT-wrapping
  ;; Exported from another package
  (let ((*results* nil))
    (with-redefs (fn-a #'(lambda () 'redef-a))
      (is (eql 'redef-a (call-fn-a))))
    (is (null *results*)))

  ;; Partially exported from another package
  ;; fn-b is exported, but not its caller.
  (let ((*results* nil)
        (expected '(redef-b 1)))
    (with-redefs (fn-b #'(lambda (x) (list 'redef-b x)))
      (is (equalp expected (with-redefs-test-aux::call-fn-b 1))))
    (is (null *results*)))

  ;; Nothing exported from another package
  (let ((*results* nil)
        (expected '(redef-c 1 2)))
    (with-redefs (with-redefs-test-aux::fn-c #'(lambda (x y) (list 'redef-c x y)))
      (is (equalp expected (with-redefs-test-aux::call-fn-c 1 2))))
    (is (null *results*)))
  )

(test basic-redefining-WITH-wrapping
  ;; Exported from another package
  (let ((*results* nil)
        (old #'fn-a))
    (with-redefs (fn-a #'(lambda () (funcall old) 'redef-a))
      (is (eql 'redef-a (call-fn-a))))
    (is (equalp '(fn-a) *results*)))

  ;; Partially exported from another package
  ;; fn-b is exported, but not its caller.
  (let ((*results* nil)
        (orig-expected '(fn-b 1))
        (expected '(redef-b 1))
        (old #'fn-b))
    (with-redefs (fn-b #'(lambda (x) (funcall old x) (list 'redef-b x)))
      (is (equalp expected (with-redefs-test-aux::call-fn-b 1))))
    (is (equalp (list orig-expected) *results*)))

  ;; Nothing exported from another package
  (let ((*results* nil)
        (orig-expected '(with-redefs-test-aux::fn-c 1 2))
        (expected '(redef-c 1 2))
        (old #'with-redefs-test-aux::fn-c))
    (with-redefs (with-redefs-test-aux::fn-c #'(lambda (x y) (funcall old x y) (list 'redef-c x y)))
      (is (equalp expected (with-redefs-test-aux::call-fn-c 1 2))))
    (is (equalp (list orig-expected) *results*))))

(test basic-redefining-WITH-wrapping-outer
  ;; Like the above, but we're wrapping the outer `call-*` functions, not the inner called functions

  ;; fn-a and call-fn-a Exported from another package
  (let ((*results* nil)
        (old #'call-fn-a))
    (with-redefs (call-fn-a #'(lambda () (funcall old) 'redef-call-fn-a))
      (is (eql 'redef-call-fn-a (call-fn-a))))
    (is (equalp '(fn-a) *results*)))

  ;; Partially exported from another package
  ;; fn-b is exported, call-fn-b is not
  (let ((*results* nil)
        (orig-expected '(fn-b 1))
        (expected '(redef-call-fn-b 1))
        (old #'with-redefs-test-aux::call-fn-b))
    (with-redefs (with-redefs-test-aux::call-fn-b
                  #'(lambda (x) (funcall old x) (list 'redef-call-fn-b x)))
      (is (equalp expected (with-redefs-test-aux::call-fn-b 1))))
    (is (equalp (list orig-expected) *results*)))

  ;; Nothing exported from another package
  (let ((*results* nil)
        (orig-expected '(with-redefs-test-aux::fn-c 1 2))
        (expected '(redef-call-fn-c 1 2))
        (old #'with-redefs-test-aux::call-fn-c))
    (with-redefs (with-redefs-test-aux::call-fn-c
                  #'(lambda (x y) (funcall old x y) (list 'redef-call-fn-c x y)))
      (is (equalp expected (with-redefs-test-aux::call-fn-c 1 2))))
    (is (equalp (list orig-expected) *results*))))

(test multiple-bindings
  "Test multiple bindings in one with-redefs form"
  (let ((*results* nil))
    (with-redefs (fn-a #'(lambda () 'redef-a)
                  fn-b #'(lambda (x) (list 'redef-b x)))
      (is (eql 'redef-a (call-fn-a))
          (equalp '(redef-b 1) (with-redefs-test-aux::call-fn-b 1)))
    (is (null *results*)))))

(defvar *x* nil "Some symbol without")
(defun foo (&rest args) 
  "Some uninteresting function whose fdefinition can be unbound."
  (setq *x* args))

(test fboundp-bindings
  "Test sym<-null-fp and null-sym<-fp bindings"
  ;; Put a function on a symbol that doesn't have one
  (flet ((beat-on-*x* ()
           (is (null (fboundp '*x*)))
           (with-redefs (*x* #'foo)
             (is (fboundp '*x*))
             ;; Funcall to avoid "undefined function" warnings you'd get with (*x* 1)
             (is (equalp '(1) (funcall '*x* 1))))
           (is (null (fboundp '*x*)))
           (is (equalp '(1) *x*))))
    (setq *x* nil)
    (beat-on-*x*)
    ;; What happens if we rebind *x*, just curious...  Should be a NO-OP for fdefinition purposes.
    (let ((*x* nil))
      (beat-on-*x*)))

  ;; Make a function unbound
  (is (fboundp 'foo))
  (with-redefs (foo nil)
    (is (null (fboundp 'foo)))
    (signals error (foo 1)))
  (is (fboundp 'foo)))

(defun bar (&rest args)
  (cons 'bar args))

(test new-function-specs
  "Testing for potentially unsupported new-function values"

  ;; This is informative.  Some lisps (e.g. LispWorks) will allow with-redefs:symbol-fn's set of
  ;; fboundp, special-operator-p, macro-function, and fdefinition, to process a symbol
  ;; and return a function. Other lisps (e.g. SBCL) will not.  I'm too lazy to figure out
  ;; the versions of the spec on this.  If `symbol-fn` allows it, we allow it, and so this
  ;; test won't work the same way on all lisps.

  ;; FMI: CL:TYPE-ERROR seems like the right error here from 
  ;; `(setf (fdefinition <sym>) <new-function>)`
  ;; but not all lisps return it. SBCL does. ACL used SIMPLE-ERROR.

  ;; *FINISH*: document all the above stuff in README.

  ;; with-redefs, that is to say `fdefinition`, requires functions, not symbols
  ;; EXCEPT ... for ABCL and LISPWORKS (of the lisps tested for WITH-REDEFS)
  #-(or abcl lispworks)
  (signals #-allegro type-error #+allegro simple-error
    (with-redefs (foo 'bar) (foo 1)))
  #+(or abcl lispworks)
  (with-redefs (foo 'bar) 
    (is (equalp '(bar 1) (foo 1))))

  ;; obvious non-function value
  (signals  #-allegro type-error #+allegro simple-error
    (with-redefs (foo 1) (foo 1)))

  ;; Checking to see whether `#'` (i.e. `function`)  is required when used with `lambda`.
  ;; Seems to be implicit for lambda for all lisps tested.
  ;; I.e. (lambda () ...) vs #'(lambda () ...) - both work with `fdefinition`
  (with-redefs (foo (lambda () 'bar))
    (is (eq 'bar (foo)))))

;;
;; Set up some setf functions for testing.
;;

(defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x))
(defun set-middleguy (x v)
  (unless (null x)
    (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
  v)
(defsetf middleguy set-middleguy)

(defstruct ship x y)
(defun ship-foo (ship) (+ (ship-x ship) (ship-y ship)))
;; Why does SETF expand such that ship is the second arg?
;; Bike: if there's no setf expander for an operator, (setf (foo x y z) w) expands
;; into (funcall #'(setf foo) w x y z). the new value always comes first (this makes
;; it easier when FOO takes a &rest parameter, for example)
(defun (setf ship-foo) (v ship) 
  (multiple-value-bind (x y) ;this is not a meaningful calculation, just debugging setf
      (truncate v 2)
    (setf (ship-x ship) (* x 2) (ship-y ship) y))
  v)
;; bar - same as foo, but with defsetf
(defun ship-bar (ship) (ship-foo ship))
(defun set-ship-bar (ship x) (funcall (function (setf ship-foo)) x ship))
(defsetf ship-bar set-ship-bar)

(test set-f         ;I suppose setf would have worked, I was leary of symbol conflict
  "Test redefinition of setf functions."
  (let ((flag nil)
        (s (make-ship :x 1 :y 2))
        (old (fdefinition '(setf ship-foo))))
    (with-redefs ((setf ship-foo) (lambda (v ship)
                                    (setf flag t)
                                    (funcall old (* v 2) ship)
                                    v))
      (setf (ship-foo s) 5)
      (is (eq t flag))
      (is (= 10 (ship-x s)))))

  (let ((flag nil)
        (s (make-ship :x 1 :y 2))
        (old (fdefinition 'set-ship-bar)))
    (with-redefs (set-ship-bar (lambda (ship v)
                                 (setf flag t)
                                 (funcall old ship (* v 2))
                                 v))
      (setf (ship-bar s) 5)
      (is (eq t flag))
      (is (= 10 (ship-x s)))))

  (let ((flag nil)
        (s (list 1 2 3))
        (old (fdefinition 'set-middleguy)))
    (with-redefs (set-middleguy (lambda (x v)
                                 (setf flag t)
                                 (funcall old x v)))
      (setf (middleguy s) 4)
      (is (eq t flag))
      (is (= 4 (middleguy s)))))
  )

(defclass ship2 () ((x :accessor ship2-x :initarg :x)))
(declaim (notinline ship-x))

(test accessors
  "Test what happens if you redefine CLOS slot accessors or structure accessors"
  ;; This declaration necessary on SBCL and probably other lisps, otherwise the defstruct
  ;; accessor is inlined and our redef has no effect.
  (declare (notinline ship-x))

  ;; Some implementations refused to cease inlining accessors despite the NOTINLINE request.
  #+(or allegro)
  (format t "This lisp does not support defstruct accessors that are NOTINLINE, a test case was skipped.~%")
  #-(or allegro)
  (let ((old #'ship-x)
        (flag nil)
        (s (make-ship :x 1 :y 2)))
    (with-redefs (ship-x (lambda (&rest args) 
                           (setf flag t)
                           (+ 1 (apply old args))))
      (is (= 2 (ship-x s)))
      (is (eq t flag))))                  ;because `is` requires form, hence `eq t`

  (let ((old #'ship2-x)
        (flag nil)
        (s (make-instance 'ship2 :x 3)))
    (with-redefs (ship2-x (lambda (&rest args) 
                            (setf flag t)
                            (+ 1 (apply old args))))
      (is (= 4 (ship2-x s)))
      (is (eq t flag)))))
  

(defmacro a-macro () "Global macro to test with redefs" `1)

(test invalid-or-local-redefs
  "defmacro/flet/labels/macrolet names and attempted redefinitions"

  ;; Can't redefine a macro
  (signals error (with-redefs (a-macro (lambda () 2)) (a-macro)))

  ;; with-redef should not even see the macrolet macro, it will bind a global function. 
  ;; The (b-macro) invocation prefers the local macro over the global function
  (let ((macrolet-flag nil)
        (lambda-flag nil))
    (macrolet ((b-macro () `(progn (setq macrolet-flag t) 2)))
      (with-redefs (b-macro (lambda () (setq lambda-flag t) 3))
        (is (fboundp 'b-macro))
        (is (= 2 (b-macro)))))
    (is (eq t macrolet-flag))
    (is (null lambda-flag)))

  ;; Does this behave like macrolet in terms of rebinding effect?
  ;; Yes. 
  (let ((flet-flag nil)
        (lambda-flag nil))
    (flet ((a-fun () (setq flet-flag t) 2))
      (with-redefs (a-fun (lambda () (setq lambda-flag t) 3))
        (is (fboundp 'a-fun))
        (is (= 2 (a-fun)))))
    (is (eq t flet-flag))
    (is (null lambda-flag)))

  (let ((labels-flag nil)
        (lambda-flag nil))
    (labels ((a-fun () (setq labels-flag t) 2))
      (with-redefs (a-fun (lambda () (setq lambda-flag t) 3))
        (is (fboundp 'a-fun))
        (is (= 2 (a-fun)))))
    (is (eq t labels-flag))
    (is (null lambda-flag)))

  ;; Test special-operators to make sure we get the expected errors.
  ;; Listed here: https://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm#clspecialops
  (signals error (with-redefs (block (lambda (&rest args) 
                                       (cons "oops!" args)))))
  (signals error (with-redefs (catch (lambda (&rest args) (cons "oops!" args)))))

  )

(defun run-tests ()
  "Run all :with-redefs tests."
  (explain! (run 'test-suite)))
