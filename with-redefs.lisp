(in-package :cl-user)

(defpackage :with-redefs
  (:use :cl)
  (:export :with-redefs)
  (:documentation "Macro modelled after the Clojure `with-redefs` namesake to rebind functions.
Tool is primarily for unit test mocking or instrumentation. YMMV, see README.md."))

(in-package :with-redefs)

#| 

See README.md for documentation.

Notes from trying to distill what I can and can't do for WITH-REDEFS.

- CLTL2: fboundp must return true for a symbol naming a macro or a special form. 

- CLTL2: fboundp is true if the symbol has a global function definition. Note that
  fboundp is true when the symbol names a special form or macro. macro-function and
  special-form-p may be used to test for these cases. 

- CLTL2: X3J13 voted in March 1989 (FUNCTION-NAME) to extend fboundp to accept any
  function-name (a symbol or a list whose car is setf-see section 7.1). Thus one may
  write (fboundp '(setf cadr)) to determine whether a setf expansion function has
  been globally defined for cadr.

- CLTL2: The function special-form-p takes a symbol. If the symbol globally names a
  special form, then a non-nil value is returned; otherwise nil is returned. A
  returned non-nil value is typically a function of implementation-dependent nature
  that can be used to interpret (evaluate) the special form.

  It is possible for both special-form-p and macro-function to be true of a
  symbol. This is possible because an implementation is permitted to implement any
  macro also as a special form for speed. On the other hand, the macro definition
  must be available for use by programs that understand only the standard special
  forms listed in table <something or other>.

- CLTL2: X3J13 voted in March 1989 (FUNCTION-NAME) to extend function to accept any
  function-name (a symbol or a list whose car is setf-see section 7.1) as well as
  lambda-expressions. Thus one may write (function (setf cadr)) to refer to the setf
  expansion function for cadr.

- CLTL2: X3J13 voted in March 1989 (FUNCTION-NAME) to add the function fdefinition to
  the language. It is exactly like symbol-function except that its argument may be
  any function-name (a symbol or a list whose car is setf-see section 7.1); it
  returns the current global function definition named by the argument
  function-name. One may use fdefinition with setf to change the current global
  function definition associated with a function-name.

- Cannot update special operators, https://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm
  See `special-operator-p`
  See `special-form-p` - renamed renamed to `special-operator-p`
  CLHS: refers to special-operator-p

- Things you can't do to CL external symbols...
  https://www.lispworks.com/documentation/lw50/CLHS/Body/11_abab.htm
  for possible additional CL spec restrictions.
|#

(defmacro with-redef-orig ((f-sym function) &body body)
  "Redefine the global function definition of symbol F-SYM
with the function FUNCTION with a lexical scope wrapping BODY.

FUNCTION must be an object of type FUNCTION, not some other function designator, 
compatible with (setf (fdefinition f-sym) <function>).

Execute BODY with the rebound function, restoring the original function (or lack thereof)
on exit.  F-SYM need not be FBOUNDP to start with.

Note that this macro may have unsafe effects in a multi-threaded use of the
symbol unless the caller arranges additional critical-section logic.
Also note compiler transformations or inlining may also result in surprises
when it comes to redefining a function, as well as use of compiled symbol-function
references that have previously nabbed the function and which won't see changes made 
after the fact.

Warning: If the new function attempts to call the old function, make sure it isn't via the
the function being redefined.  E.g.

  ;; This will be an infinite loop or stack overflow
  (with-redef (my-fun (lambda () ... stuff ... (funcall 'my-fun))))

  ;; ;This will work
  (let ((old-fun #'my-fun))
    (with-redef (my-fun (lambda () ... stuff ... (funcall old-fun)))))

Returns the value(s) returned by BODY."
  ;; Don't really like this implementation, it'll do for the limited test here.
  ;; Would prefer to unwind-protect the setting of the symbol function as well as the 
  ;; restoration, among other things.  Also, we should probably use FDEFINITION
  ;; instead of SYMBOL-FUNCTION, so we can do SETF functions as well as plain symbols.
  ;; Or something like that.
  ;; Note effect on generic functions/methods?
  (let ((old (gensym))
        (fun (gensym)))
    (declare (ignorable old))
    `(let ((,fun ,function))
       (assert (typep ,fun 'cl:function))
       (cond
         ((fboundp ',f-sym)
          (let ((,old (symbol-function ',f-sym)))
            (setf (symbol-function ',f-sym) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',f-sym) ,old))))
         (t
          (setf (symbol-function ',f-sym) ,fun)
          (unwind-protect (progn ,@body)
            (fmakunbound ',f-sym)))))))

(defun setf-name-p (function-name)
  "Return true if function-name is of the form `(cl:setf symbol)`."
  (and (listp function-name)
       (eq 'cl:setf (car function-name))
       (let ((second (second function-name)))
         (and second                    ;since nil is a symbol
              (symbolp second)))))

(defun symbol-fn (function-name)
  "Given a function name, which is a symbol naming a function or
`(setf symbol)`, return the _global function_ associated with the function name.

Functions associated with the name due to FLET, LABELS, or MACROLET are not
considered.

If the function is not FBOUNDP, return NIL.
If the function is a special operator or macro, return a keyword, either
:SPECIAL-OPERATOR or :MACRO."
  (cond ((fboundp function-name)
         (if (setf-name-p function-name) ;(setf <symbol>)
             (fdefinition function-name)
             (cond ((special-operator-p function-name) :SPECIAL-OPERATOR)
                   ((macro-function function-name) :MACRO)
                   (t (fdefinition function-name)))))
        (t nil)))

(defun bind-symbol-fn (function-name new-function)
  "Bind the fdefinition of FUNCTION-NAME to NEW-FUNCTION.
If NEW-FUNCTION is NIL, make the symbol such that (FBOUNDP symbol) is NIL.

NEW-FUNCTION must be a function as per the FUNCTION special form, the function COERCE,
or the function COMPILE.

Returns NIL."
  (if (null new-function)
      (fmakunbound function-name)
      (setf (fdefinition function-name) new-function))
  nil)

(defun with-redefs-aux (function-names new-functions f)
  "Bind all function-names (as per `symbol-fn`) to the corresponding new functions
(as per `bind-symbol-fn`), invoke `f`, and
restore all function definitions on exit.  Returns the value of (f)."
  (let* ((old-funs (mapcar #'symbol-fn function-names)) ;=> function, NIL, :MACRO, :SPECIAL-OPERATOR
         (invalid-funs
           (loop for function-name in function-names
                 for old-fun in old-funs
                 when (keywordp old-fun)
                   collect (list function-name old-fun))))
    (when invalid-funs
      (error "The following function name~P cannot be bound:~%~{~{~s, is a ~a~}~^, ~}" 
             (length invalid-funs) invalid-funs))
    (unwind-protect
         (progn
           (map nil #'bind-symbol-fn function-names new-functions)
           (funcall f))
      (map nil #'bind-symbol-fn function-names old-funs))))

(defmacro with-redefs ((&rest bindings) &body body)
  "Given one or more bindings such that

    binding => function-name new-function

temporarily set the global function definition of each function-name to the corresponding
new-function and execute BODY, returning whatever BODY returns after restoring the
original function definitions.

Each function-name should be a symbol naming a global function or a list of the form
`(setf symbol)`.  Functions associated with FLET, LABELS, or MACROLET will be ignored,
only the global function names are searched and bound.

Macros (for which `macro-function` is non-nil) and special operators (for which
`special-operator-p` is true) may not be rebound and will result in an error being signalled.

New-function values may be NIL, in which case the function-name will be made such that
(FBOUNDP function-name) is NIL for the duration of BODY, otherwise all new-function values must
be functions as per the FUNCTION special form (a.k.a. `#'`), the function COERCE,
or the function COMPILE.

All new-function values in the bindings are evaluated in before any redefinition activity
occurs.

WITH-REDEFS is useful for mocking or wrapping functions being tested.
This macro should not be used in production and may not work for all functions or
Common Lisp implementations.  See README.md for additional caveats. Works only for function
definitions, does not deal with symbol-value bindings.

Example:
    (defun foo () (print 'foo))

    (with-redefs (foo #'(lambda () (print 'bar)))
      (foo))

    => <prints 'bar>
"
  (loop with function-names = nil
        with new-functions = nil
        for (function-name new-function) on bindings by #'cddr
        do (push function-name function-names)
           (push new-function new-functions)
        finally (return `(with-redefs-aux ',(reverse function-names)
                           (list ,@(reverse new-functions))
                           #'(lambda () ,@body)))))


