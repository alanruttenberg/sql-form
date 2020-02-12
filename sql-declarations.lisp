(in-package sql-form)

(defclass sql-name-manager ()
  ((parsed-columns :accessor parsed-columns :initarg :parsed-columns)
   (parsed-tables :accessor parsed-tables :initarg :parsed-tables)))


;; Rules
;; Bindings allow aliasing (for readability) of tables and columns.
;; If one column is aliased, default is to inherit that alias with same-named columns
;; When declaring a column you can use an earlier alias

;; when we're done, we have a list of
;; (:table table-alias table-name db-alias)
;; and
;; (:column table column-alias column-name)
;; Where aliases aren't given the alias is the same as the name
;; if table-name=db-alias then no db-alias should be used.
(defmethod parse-declarations ((n sql-name-manager) tables)
  (flet ((maybe-already-aliased (column columns)
	   (let ((already (find column columns :key 'third)))
	     (if already
		 (cddr already)
		 (list column column))))
	 (add-table-aliases-if-necessary (tables)
	   (loop with used-db-aliases = (remove nil (mapcar 'fourth tables))
	 	 for (nil table-alias table-name db-alias) in tables
	 	 if (and (and (null db-alias) (not (member table-name used-db-aliases ))))
	 	   collect (list :table table-alias table-name table-name)
	 	   and do (push table-name used-db-aliases)
	 	 else collect (loop for i from 2
				    for sym = (intern (format nil "~a~a" (string table-name) i))
				    until (not (member sym used-db-aliases))
				    finally (progn (push sym used-db-aliases) (return (list :table table-alias table-name sym))))))
	 )
    (loop for (table . columns) in tables
	  if (consp table) 
	    collect (list* :table table) into parsed-tables
	  else collect (list :table table table) into parsed-tables
	  append (loop for column in columns
		       if (consp column) collect (list :column (if (consp table) (second table) table)  (first column) (second column))
			 else collect
			      (if (find #\- (string column))
				  (list :column (if (consp table) (second table) table) column (intern (#"replaceAll" (string column) "-" "") (symbol-package column)))
				  (list* :column (if (consp table) (second table) table)
					 (maybe-already-aliased column parsed-columns)) ))
	    into parsed-columns
	  finally 
	     (progn
	       (setf (parsed-columns n) parsed-columns)
	       (setf (parsed-tables n) (add-table-aliases-if-necessary parsed-tables))
	       (return n)))))


  

'(defun test ()
  (pprint (parse-declarations '(((form-item fitem)
				 (item-key fitem) (form-item-deleted deleted) (form-reference-code formcode))
				( fitem
				 (item-key fitem) (form-item-deleted deleted) (form-reference-code formcode))
				((form-item-required fitemreq)
				 item-key (form-status fstatus))
				((filled-form-item pitem)
				 item-key (filled-form-key pform) (filled-form-item-status status))
				((filled-form pform)
				 (filled-form-inactive inactive) filled-form-key)
				((template comment2)
				 (template-reference-code code) isnode form-reference-code)))))


;resolve :column
;if dotted split, lookup table, (last lookup column )

;(:table alias name db-alias)
;(:column table column-alias column-name)


(defmethod resolve-column ((n sql-name-manager) column &aux table table-name column-name)
  (if (find #\. (string column))
      (let* ((split (mapcar 'intern (split-at-char (string column) #\.))))
	(setq table (first split) column (second split))))
					;  at this point table and column can be aliases
					;  if we have table, then get name
  (if table
      (assert (setq table-name (or (third (find table (parsed-tables n) :key 'second))
				   (third (find table (parsed-tables n) :key 'fourth))))
	      ()
	      "Couldn't find table name for ~a - did you use table name intead of alias?" table)
      (assert (setq table-name (second (find column (parsed-columns n) :key 'third)))
	      ()
	      "Couldn't find table for column ~a" column))
  (cl-user::print-db column table table-name)    
  (setq table-alias (fourth (find table-name (parsed-tables n) :key 'third)))
  ;; now normalize column
  (assert (setq column-name (fourth (find-if (lambda(el) (and (eq (second el) table-name) (eq (third el) column)))
					     (parsed-columns n))))
	  ()
	  "Couldn't find column ~a" column)
  (list table-name column-name)
  )

(defmethod resolve-table ((n sql-name-manager) table &aux  table-name )
  (assert (setq table-name (or (third (find table (parsed-tables n) :key 'second))
			       (third (find table (parsed-tables n) :key 'fourth))))
	  ()
	  "Couldn't find table name for ~a - did you use table name intead of alias?" table)
  table-name)
    
(defmethod render-column ((n sql-name-manager) table-name column-name)
  (let ((*print-case* :downcase))
    (format nil "~a.~a" table-name column-name)))

(defmethod render-table ((n sql-name-manager) table-name &optional for-on?)
  (let ((*print-case* :downcase))
    (format nil "~a" table-name)))

(defmethod render-coded ((n sql-name-manager) keyword column &optional table)
  (error "Don't know how to decode keyword ~a" keyword))


