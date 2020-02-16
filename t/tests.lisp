(in-package :cl-user)
(defpackage sql-form-test
  (:use :cl :prove :sql-form))
(in-package :sql-form-test)


(defmacro with-setup (declarations &body body)
  `(let ((n (make-instance 'sql-name-manager)))
     (progn (parse-declarations n ',declarations)
	    ,@body)))
  
(defun equals-case-space-independent (a &rest b)
  (equal (string-downcase (#"replaceAll" a "(?s)\\s{2,}" " "))
	 (string-downcase (#"replaceAll" (apply 'concatenate 'string b) "(?s)\\s{2,}" " "))))

(plan 20)

(ok (make-instance 'sql-name-manager)
    "instantiate name manager")

(is :tab
    (with-setup  ((tab)) (table-key (first (tables n))))
    "Check table key")

(is 2
   (with-setup (((f1 f2 tab)))
     (length (tables n)))
    "two table aliases yield 2 tables")

(is "tab.col"
    (with-setup ((tab col))
      (render-column n (car (columns (car (tables n))))))
    "render column")

(is "'a'"
    (with-setup ((tab col))
      (render-literal n "a"))
    "render literal")

(ok (equals-case-space-independent
     (sql-form-to-string  '((tab col1 col2)) 
			 '(:select () (col1 col2)
			   (:where (= col1 col2)))
			 nil)
    "SELECT tab.col1, tab.col2 FROM tab WHERE tab.col1 = tab.col2")
    "select where implicit from")

(ok (equals-case-space-independent
     (sql-form-to-string  '((tab col1 col2)) 
			 '(:select () (col1 col2)
			   (:from tab)
			   (:where (= col1 col2)))
			 nil)
    "SELECT tab.col1, tab.col2 FROM tab WHERE tab.col1 = tab.col2")
     "select where explicit from")

(ok (equals-case-space-independent 
     (sql-form-to-string  '((tab col1 col2)) 
			 '(:select () (col1 col2)
			   (:where (= col1 col2))
			   (:from tab))
			  nil)
    "SELECT tab.col1, tab.col2 FROM tab WHERE tab.col1 = tab.col2")
    "order of from and where doesn't matter")

(ok (equals-case-space-independent
     (sql-form-to-string  '((tab1 col1) (tab2 col2)) 
			 '(:select () (col1 col2)
			   (:from tab1
			    (:on tab2 (= col1 col2))))
			 nil)
    "SELECT tab1.col1, tab2.col2 FROM tab1 INNER JOIN tab2 ON tab1.col1 = tab2.col2")
     "inner join distinct column names")

(ok (equals-case-space-independent
     (sql-form-to-string  '((tab1 col) (tab2 col)) 
			 '(:select () (tab1.col)
			   (:from tab1
			    (:on tab2 (= tab1.col tab2.col))))
			 nil)
    "SELECT tab1.col FROM tab1 INNER JOIN tab2 ON tab1.col = tab2.col")
    "inner join same column in two tables")

(ok (not (equals-case-space-independent 
       (sql-form-to-string  '(((tab1 tab2 tab) col target))
			 '(:select () (tab1.target)
			   (:from tab1
			    (:on tab2 (= tab1.col tab2.col))))
			 nil)
      "SELECT tab1.target FROM tab1 INNER JOIN tab2 ON tab1.col = tab2.col"))
    "doesn't define table alias")

(is-error (sql-form-to-string  '(((tab1 tab2 tab) col target))
			       '(:select () (target)
				 (:from tab1
				  (:on tab2 (= tab1.col tab2.col))))
			       nil)
	  'simple-error
	  "doesn't qualify ambiguous column")


(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) col target))
			  '(:select () (tab1.target)
			    (:from tab1
			     (:on tab2 (= tab1.col tab2.col))))
			  nil)
     "SELECT tab1.target FROM tab as tab1 INNER JOIN tab as tab2 ON tab1.col = tab2.col")
    "table aliases")

(ok (equals-case-space-independent 
     (sql-form-to-string  '(((tab1 tab2 tab) col )
			   ((table-3 tab3) col target))
			 '(:select () (target)
			   (:from tab1
			    (:on table-3 (= table-3.col tab1.col))))
			 nil)
     "SELECT tab3.target FROM tab as tab1 INNER JOIN tab3 ON tab3.col = tab1.col")
    "check table alias no db-alias")


(is-error (sql-form-to-string  '(((tab1 tab2 tab) col )
				 ((table-3 tab3) col target))
			 '(:select () (target)
			   (:from tab1
			    (:on table-3 (= table-3.col col))))
			       nil)
	  'simple-error
	  "ambiguous use of column (col)")

(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) (column colm))
				 ((table-3 tab3) column target))
			       '(:select () (target)
				 (:from tab1
				  (:on table-3 (= table-3.column tab1.column))))
			       nil)
	  "SELECT tab3.target FROM tab as tab1 INNER JOIN tab3 ON tab3.column = tab1.colm")
    "column alias same as another column name")

(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) (column colm))
				 ((table-3 tab3) column target))
			       '(:select () (target)
				 (:from tab1
				  (:on table-3 (:using column))))
			  nil)
     "SELECT tab3.target FROM tab as tab1 INNER JOIN tab3 ON tab1.colm = tab3.column")
    "test join :using syntax ")

(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) (column colm))
				 ((table-3 tab3) column target))
			       '(:select () (target)
				 (:from tab1
				  (:on table-3 (:via column tab1))))
			  nil)
     "SELECT tab3.target FROM tab as tab1 INNER JOIN tab3 ON tab3.column = tab1.colm")
    "test join :via syntax")

(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) (column colm))
				 ((table-3 tab3) column target))
			       '(:select () (target)
				 (:where (like-insensitive target "foo%" "bar%")) 
				 (:from tab1
				  (:on table-3 (:via column tab1))))
			  nil)
     "SELECT tab3.target FROM tab as tab1 INNER JOIN tab3 ON tab3.column = tab1.colm WHERE UPPER(tab3.target) "
     "LIKE 'FOO%' OR UPPER(tab3.target) LIKE 'BAR%'")
    "like-insensitive")

(ok (equals-case-space-independent
     (sql-form-to-string  '(((tab1 tab2 tab) (column colm))
				 ((table-3 tab3) column target))
			       '(:select () (target)
				 (:where (like target "foo%" "bar%")) 
				 (:from tab1
				  (:on table-3 (:via column tab1))))
			  nil)
     "SELECT tab3.target FROM tab as tab1 "
     "INNER JOIN tab3 ON tab3.column = tab1.colm "
     "WHERE tab3.target LIKE 'foo%' OR tab3.target LIKE 'bar%'")
    "like")
     

(finalize)
