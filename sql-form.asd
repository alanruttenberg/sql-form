;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :sql-form
  :components
  ((:file "package")
   (:org "sql-name-manager")
   (:org "sql-render")
   (:file "define-query-function")
   (:module tests :pathname "t/" :serial t
    :components ((:test-file "tests")
		 ))
   )
  :depends-on (gsp prove prove-asdf)
  :defsystem-depends-on (lilith prove-asdf)
  :serial t)

(format t "To test sql-form: (uiop:symbol-call :prove-asdf 'run-test-system :sql-form)")

;;;; eof
