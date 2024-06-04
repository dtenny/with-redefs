# About

`WITH-REDEFS` is a macro that allows you to rebind the _global_ functions associated
with symbols. It is inspired by the Clojure macro of the same name, however
the nature of Common Lisp prohibits it from functioning identically.

The tool is used mainly for mocking functions in test environments, or
wrapping functions so that you can intercept and record their activity,
also used primarily for testing.

*THIS IS A TEST TOOL, YOU SHOULD NOT USE IT IN CODE THAT GOES TO PRODUCTION ENVIRONMENTS*

I've packaged this up as an independent project/repo because unlike Clojure
where redefining a function is very easy, Common Lisp is chock full of
caveats.  A compiled lisp follows a different set of rules than Clojure's
compile-on-the-fly-after-loading JVM environment. That said, Common Lisp's
compilation model also has strengths that help ensure your code will work
in production *before* it gets to production. Contrast this to Clojure,
where I could bundle up my grandmother's cookie recipes in an uberjar and
it might not fail until it's invoked by some other library dependent on it
when it is executed in production.

Also (why I put this in distinct repo), I haven't released a general
purpose clojure compatibility library that would be suitable for this
macro, however it's useful enough I wanted it where I could quickload it
for ad-hoc use and/or just share this documentation. The implementation is
trivial, it's the documentation and tests that took a bit of time.

## Usage

This lisp system is available via the Ultralisp distribution in QUICKLISP.

If you didn't get this via quickload a quicklisp/ultralisp repo, add it to
your `~/quicklisp/localprojects/` directory, update/delete the
`system-index.txt` file accordingly, and then you can quickload it.
(Quicklisp rebuilds the `system-index.txt` if it is missing).

    ;; To load just the `WITH-REDEFS` package
    (ql:quickload :with-redefs)

    ;; To load `WITH-REDEFS` and `WITH-REDEFS-TEST` packages
    (ql:quickload :with-redefs-test)
    (with-redefs-test:run-tests)

Then you can:

    (with-redefs (some-global-function-name replacement-function
                  ...) ; possible additional symbol/replacement-function pairs
      ... body ...) ; returns value of body

See examples below and test module for more examples.

## Tested Lisps and implementation-specific notes

All tests were run at default compiler optimization settings. YMMV.

- SBCL: WORKS

- CCL 1.12.2: WORKS

- Allegro CL Express 11.0 (`alisp` executable): WORKS

  Unable to test NOTINLINE defstruct accessors.

  Allegro also does not return TYPE-ERROR where the spec appears to require it, noted
  in one conditionalized test.

- ECL 21.2.1: WORKS

- ABCL 1.9.0, OpenJDK 17.0.9: WORKS - with style warnings

  It might be nice to use `MUFFLE-WARNING` for the style warnings on
  redefined functions, but for not that's left to the caller. Only ABCL
  issued such style warnings of the lisps tested here.

  Also allows symbols for new-function, though you should consider this non-portable.

- LispWorks 8.0.1 Personal Edition: WORKS

  Also allows symbols for new-function, though you should consider this non-portable.


## Examples and some caveats

    (defun my-function (x y)
      (format t "MY-FUNCTION called with args ~s ~s~%" x y)
      (+ x y))

    ;; Simple replacement of a pre-existing function associated with the symbol MY-FUNCTION
    (with-redefs (my-function 
                  (lambda (&rest args)
                    (format t "MY-FUNCTION redef called with args ~s~%"
                            args))) ;returns nil
      (my-function 1 2 3))

    MY-FUNCTION redef called with args (1 2 3)
    => NIL

Note that `WITH-REDEFS` uses `UNWIND-PROTECT` to ensure restoration the
original function value.

    ;; Wrapping a pre-existing function
    (let ((old-function #'my-function))   ;NOTE: `FUNCTION` (i.e `#'`) is important
    (with-redefs (my-function 
                  (lambda (&rest args)
                    (format t "MY-FUNCTION redef called with args ~s~%" args)
                    (funcall old-function (first args) (second args))))
      (my-function 1 2 3))

    MY-FUNCTION redef called with args (1 2 3)
    MY-FUNCTION called with args 1 2
    => 3

Note that because the symbol-function of the symbol is rebound by `WITH-REDEFS`
callers of the function will not see the changed function slot unless they are 
either:
    
a. calling the function in the normal function position of a form, e.g. `(my-function 1 2)` or
b. calling the function via symbol, e.g. `(funcall 'my-function 1 2)`.

However if some caller is calling via `(funcall #'my-function 1 2)` they are
not going to see the redefined symbol-function value because it was
obtained prior to the `with-redefs` update of the symbol function.

You may ask "why can `CL:TRACE` instrument functions that `WITH-REDEFS`
can't?" even though they are also called with `#'my-function`.  The answer
is that `CL:TRACE` _may_ know enough about the implementation to update the
actual compiled code vector so it doesn't need to change the
symbol-function slot.

These effects can vary by platform.  In all likelihood if you want to
redefine a function using `WITH-REDEFS`, you should plan for it, making
sure *NOT* to use `#'my-function` (which is the same as `(function
my-function)`).

That may or may not be a small performance hit (having to
deref the symbol-function slot) depending on lisp implementation and
context, but it's nicer than having test-mode-only logic cluttering the
function(s) in question.

## More Examples

One of my favorite uses of WITH-REDEFS in testing is to be able to assert
whether or not a function was called.  E.g.

    ;; in the production code that I do NOT want to leave permanently
    ;; instrumented for testing
    (defun function-a ()
      ...)

    (defun function-b (thing)
      (if thing
          (function-a)
          (function-b)))

    ;;; in the test module
    (test 
      (let ((counter 0)
            (old #'function-a))
        (with-redefs (function-a 
                      (lambda () 
                        (incf counter)
                        (funcall old)))
           ... stuff that calls function-b ...
           ;; Ensure that function-a was/was-not called as per expectations
           (is (= <truth> counter)))))

If your functions are called in a multi-threaded system, ensure you 
use an atomic or otherwise mutexed counter.

## SETF update functions

You can also redefine setf updaters (subject to general caveats outlined
below), see the `set-f` test for examples in `with-redefs-test.lisp`.

## More caveats

### Inlined functions, `(DECLAIM (NOTINLINE F))`

Common Lisp implementations (of which there are many), compiled lisps that
may utilize both static and dynamic compilation tricks, also have different
ideas about the realm of optimizations that can be applied to code.

Users have some control over this with the `OPTIMIZE` declarations, however
even if you expect that you are not running in super-optimized modes, the
compiler may still inline your functions, for example if the function being
called is not called or exported outside of the module that defines it.

If your implementation aggressively inlines a function you want to
redefine, try `(declaim (notinline my-function))` in the compilation unit
and see if that helps.  Note that `defstruct` accessors are
high-probability candidates for inlining unless directed otherwise.

Note that in some lisps declaring a defstruct accessor notinline is not enough
(with with a global DECLAIM, or a local DECLARE in the test), e.g. Allegro.
Allegro's `describe` says:

    WITH-REDEFS-TEST(5): (describe 'ship-x)
    SHIP-X is a NEW SYMBOL.
      It is unbound.
      It is INTERNAL in the WITH-REDEFS-TEST package.
      Its function binding is
        #<Closure SAFE-DEFSTRUCT-ACCESSOR [SHIP] @ #x10007830ad2>
        which function takes arguments (OBJECT)
      Its property list has these indicator/value pairs:
    SYSTEM::.INLINE.            NOTINLINE
    EXCL::SETF-INVERSE          (EXCL::DEFSTRUCT-SLOT-DEFSETF-HANDLER . 1)
    SYSTEM::LISP-DUAL-ENTRY     SYSTEM::STRUCT-ACCESSOR-HOOK
      Its source-file location is: /home/dave/CommonLisp/with-redefs/with-redefs-test.lisp

So it appears to see something about "notinline" but isn't acting on it.
I wasn't able to determine if there's a workaround for Allegro with a
cursory search.

### Package-locked functions

`WITH-REDEFS` does not attempt to unlock locked packages, so any attempt to
redefine a symbol in those packages will likely fail.

### COMMON-LISP package functions

Package locks aside, it is probably a bad idea to try to redefine functions
in the `COMMON-LISP` package.

### Compile-time effects

Beware compile-time effects/interactions.  For example if you call
`EVAL` in the body of your `WITH-REDEFS`, and the eval causes the function
you've redefined to be recompiled, then your redefinition will likely be lost.

### WITH-REDEFS is not thread-safe

The symbol whose symbol-function is being redefined is not manipulated
under the control of a mutex. This is fine if you're doing single-threaded
test logic.

### WITH-REDEFS does not update symbol-value slots

Clojure, being a lisp-1, doesn't distinguish between symbol-value and
symbol-function slots.  Common Lisp, being a lisp-2, has both, and
`WITH-REDEFS` updates _only_ the symbol-function slot.

### `FDEFINITION` is key, implementations vary

At the end of the day, a global function definition is side-effected
through the use of `(SETF (FDEFINITION FUNCTION-NAME) NEW-FUNCTION)`.
According to the standard the `NEW-FUNCTION` argument must be a `FUNCTION`,
which is what is returned by the `FUNCTION` special form, the function
`COERCE`, or the function `COMPILE`.

Most tested lisps implement this and will not accept a symbol for
`NEW-FUNCTION`.  Some lisps such as ABCL and LispWorks do accept
symbols. `WITH-REDEFS` doesn't judge, whatever `SETF (FDEFINITION X)`
accepts is fine, but be aware of the differences for portability, if you care.

Note that one difference between `WITH-REDEFS` and `SETF (FDEFINITION X)`
is that `WITH-REDEFS` take symbols which are not `FBOUNDP` and turn them
into symbols which _are_ `FBOUNDP`, and vice versa.

See also
[FDEFINITION](https://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm)
Hyperspec content.

### You may not rebind macro and special operator definitions

`WITH-REDEFS` uses the CL functions FBOUNDP, SPECIAL-OPERATOR-P, and
MACRO-FUNCTION to try to signal an error if you attempt to rebind macros
and special operators. Your mileage may vary, hopefully the logic is
portable from a standards compliant standpoint though.  Some of these
functions, like SPECIAL-OPERATOR-P underwent changes in the standards work. 

See also
[Common Lisp external symbol constraints](https://www.lispworks.com/documentation/lw50/CLHS/Body/11_abab.htm)
in the hyperspec.

