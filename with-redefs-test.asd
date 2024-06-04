(in-package :cl-user)

(defpackage :with-redefs-test-asd
  (:use :cl :asdf))

(in-package :with-redefs-test-asd)

(defsystem :with-redefs-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for the :with-redefs package."
  :depends-on (:with-redefs :fiveam)
  :components ((:file "with-redefs-test-aux")
               (:file "with-redefs-test")))
