(in-package :cl-user)

(defpackage :with-redefs-asd
  (:use :cl :asdf))

(in-package :with-redefs-asd)

(defsystem :with-redefs
  :version "1.0.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Clojure-style `with-redefs` for rebinding symbols naming functions."
  :bug-tracker "https://github.com/dtenny/with-redefs/issues"
  :source-control (:git "https://github.com/dtenny/with-redefs")
  :serial t
  :components ((:file "with-redefs")))
