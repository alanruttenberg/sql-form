(require 'jdbc)

(in-package :sql-form)
	    
;; helper function. Defines a function that either runs the query or prints the
;; query (:debug t). If running the query then (:save-to <filename>) will save it
;; or print it (:print t). Database can be specified with (:db <database>) where
;; database is :test or :production

;; The first argument to the created function are the arguments to render-sql
;; The second argument is an options filter - can rewrite results.
;; filter gets called on each row and returns the possibly modified row.
;; If filter-additional-args are passed they are passed as arguments to filter-fn after row

;; Attend to two schema options
;; :trim t removes leading and trailing spaces in query results
;; :empty-is-nil t changes empty strings "" to nils


(defmacro define-query (name args render-sql-args &optional filter filter-additional-args)
  (let ((sql-args (make-symbol "SQL-ARGS")))
    `(defun ,name (,@args ,@(if (not (member '&key args)) '(&key)) (db :test) save-to debug print )
       (let ((,sql-args ,render-sql-args))
	 (define-query-internal ,sql-args db save-to debug print ,filter ,filter-additional-args)))))

(defun define-query-internal (sql-args db save-to debug print filter filter-additional-args)
  (when (eq debug :all)
    (progn (let ((*print-case* :downcase)) (pprint sql-args))
	   (terpri)(terpri)))
  (multiple-value-bind (query name-manager) (apply 'sql-form-to-string   sql-args)
    (let* ((selected-columns  (third (second sql-args)))
	  (coded-columns (get-column-codes name-manager selected-columns))
	  (select-options (second (third sql-args)))
	  (options (default-schema-options (third sql-args))))
      (if debug
	  (debug-query sql-args debug name-manager) 
	  (define-query-do-query query select-options selected-columns db save-to print options filter filter-additional-args coded-columns )
	  ))))

(defun debug-query (sql-args debug-type name-manager)
  (let ((*print-case* :downcase)) 
    (let ((query (apply 'sql-form-to-string sql-args)))
      (case debug-type
	(:all
	 (pprint (expect-select (second sql-args)))
	 (princ query)
	 (terpri)(terpri))
	(:string query)
	(:document
	 (loop for (table description columns) in (descriptions-by-table name-manager)
	       do
		  (format t "~%Table ~a: ~a~%" (table-key table) (or description "" ))
		  (loop for (column description) in columns
			do
			   (format t "  ~a: ~a~%" (column-key column) (or description "")))))
	(otherwise 
	 (princ query)
	 (values))))))

(defun get-column-codes (name-manager columns)
  (setq @ name-manager)
  (loop for column in columns
	for codes = (and (symbolp column)
			 (codes (resolve-column name-manager column)))
	when codes collect (list column codes)))
	  

(defun define-query-do-query (query select-options selected-columns db save-to print options filter filter-additional-args coded-columns)
    (with-jdbc-connection 
	(lambda(connection)
	  (declare (ignore connection))
	  (let ((result (sql-query query :cache (not (or save-to print)))))
	    (when (getf options :trim)
	      (setq results (trim-results result)))
	    (let ((filtered-result 
		    (if filter
			(loop with filterfn = filter
			      with additional-args = filter-additional-args
			      for row in result
			      for filtered = (apply filterfn row additional-args)
			      when filtered collect filtered)
			result)))
	      (setq filtered-result (maybe-decode-coded-types filtered-result coded-columns))
	      (if (not (or print save-to))
		  (if (getf  select-options :flatten) (mapcar 'car filtered-result) filtered-result)
		  (progn
		    (when print
		      (let* ((*print-case* :downcase)
			     (headers (mapcar (lambda(e) (keywordify (if (consp e) (second e) e))) selected-columns))
			     (seps (mapcar (lambda(e) (intern (substitute-if #\- 'identity e))) (mapcar 'string headers))))
			(format-as-table (list* headers seps
						filtered-result))))
		    (when save-to
		      (with-open-file (f save-to :direction :output :if-exists :supersede)
			(loop for row in filtered-result do (format f "~{~a~^	~}~%" (if (consp row) row (list row))))))))))) 
      (funcall (getf options :connection-string-function) db)))

(defun trim-results (results)
  (mapcar (lambda(r)
	    (mapcar (lambda (c)
		      (if (null c)
			  nil
			  (let ((trimmed (string-trim " " c)))
			    (if (and (getf options :empty-is-nil)
				     (equal trimmed ""))
				nil
				trimmed)))) r)) result))

(defun maybe-decode-coded-types (results selected-columns coded-columns)
  (if coded-columns
      (let ((column-coding 
	      (loop for column in selected-columns
		    for codes = (and (symbolp column) (second (find column coded-columns :key 'car)))
		    collect (list codes (if codes (if (numberp (caar codes)) 'number 'string))))))
	(loop for row in results
	      collect
	      (loop for field in row
		    for (codes type) in column-coding 
		    if codes
		      collect (if (numberp type) 
				  (second (find (parse-integer field) codes :key 'car :test 'eql))
				  (second (find field codes :key 'car :test 'string-=)))
		    else collect field)))
      results))

