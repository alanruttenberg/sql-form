;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defsystem :sql-form
  :components
  ((:file "package")
   (:file "sql-declarations")
   (:org "sql-render")
   )
  :depends-on (gsp)
  :defsystem-depends-on (lilith)
  :serial t)

;;;; eof
