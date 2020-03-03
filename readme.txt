This provides a way to write sql queries as sexp, with a number of niceties.

Here's an example:

```lisp
(define-query pform-answers (pform)
  `(((pform pform formcode inactive  fstatus)
     (pitem fitem pform pitem entrydatetime time status auditnumber pitemcol user appuser appdatetime)
     (fitem formcode fitem deleted text type)
     (fitemreq fitem fstatus)
     (form code)
     ((form-user approval-user users) level user)
     ((user-slevel approval-slevel slevel) slevel description))
    (:select () (("item#" fitem.fitem)
		 ("required in" (:list-of-column fitemreq.fstatus (= fitem.fitem fitemreq.fitem)))
		 ("ans#" auditnumber)
		 ("user level" user-slevel.description)
		 ("approver level" approval-slevel.description)
		 ("answered" entrydatetime)
		 ("approved" appdatetime)
		 ("question" fitem.text ))
      (:from pform
	     (:on form (= pform.formcode form.code))
	     (:on fitem (= form.code fitem.formcode))
	     (:left-on pitem
			(= pitem.pform pform.pform)
			(= pitem.fitem fitem.fitem)
			)
	     (:left-on form-user (:via user pitem))
	     (:left-on approval-user (= appuser approval-user.user))
	     (:left-on user-slevel (= form-user.level user-slevel.slevel))
	     (:left-on approval-slevel (= approval-user.level approval-slevel.slevel)))
      (:where (= pform.pform ,pform)
	      (= deleted :no)
	      (= inactive :no)
	      (not (in fitem.type :page-label :header :spreadsheet-heading))
	      )
      (:order-by entrydatetime))
    (:defaults :axium :trim t)))
```
	
```lisp 
(define-query name arguments `(declarations query defaults) &optional filter filter-additional-args)
```

Defines a function that executes query. Declarations let you say
which tables and columns are to be used, and to define aliases for them
either for disambiguation, readability, or in the case of tables, 
to be able to do, e.g. self-joins.

Creates a function (defun name (arguments &key print debug) ...)

Arguments, if passed, can be used to splice in to the backquote template.

The declarations are interpreted by an instance of sql-name-manager 
or subclass. The name manager is responsible for turning column and
table names in the query into how they are named in the database.

The function sql-query in jdbc.lisp is used to execute the query.
Without keywords calling function with arguments executes the query, 
returning a list of rows, each of which is a list of columns. Values
are somewhat dwimmed - the various character types come back as strings,
the numeric types as numbers. Blobs are a WIP. Other types may signal an 
error in which case more work needs to be done in jdbc.lisp to handle them.

If passed filter, it should be a function. The function will be
called on each row, and should return a row. It can change values in
the row, or combine columns, or whatever. If it returns nil 
the row is omitted. If filter-additional-args are given they
are passed as additional arguments after the row. 
Filter function should take a keyword argument :headers. The filter
function will be called with :headers t once and should return 
headers for the filtered rows.

Query results are cached with the key being the connection string and
the text of the query.

:print t will print results as a table.
:print :org will print results as a table suitable for insertion into org mode.
:debug t will print out the generated SQL.
:debug :all will show some intermediates too.
:force t will not use the cached value, if there is one.
:document t will document tables and columns used in the query, if 
   the name manager supports that.
:db keyword will do the query against the database associated with keyword (see below)
:save-to file will save the results as csv.

declarations = (table-decl column-decl*)*
table-decl = table-symbol | (table-alias* table-symbol)
column-decl = column-symbol | (column-alias column-symbol)
column-symbol = symbol | symbol.symbol
table-symbol, table-alias, column-alias = symbol
table-name = table-symbol | table-alias

query = (:select select-options (selected-column*) from-clauses? where-clauses? order-by? group-by?)
select-options = (limit-option? flatten-option)
limit-option = :limit <number>
flatted-option = :flatten t
selected-column = column-name | column-expression | (column-header column-symbol) | (column-header column-expression)
column-name = column-symbol | * | rownum
column-header = <string>

column-expression = sql-expression | selects-expression? | list-of-columns-expression | minmax-expression
selects-expression = (:selects? (selected-columns) from-clauses? where-clauses?)
list-of-columns-expression = (:list-of-columns column-symbol where-clauses?)
minmax-expression = (:at-min-within|:at-max-within sql-expression minmax-column group-column)
minmax-column, group-column = column-name 

where-clauses = sql-condition*

sql-condition = (operator sql-expression*) | (and|or sql-condition+) | (not sql-condition)
operator = + | - | < | > | | <= | >= | null | in-condition | like-condition | minmax-condition 
in-condition = (:in column-name sequence-expression)
like-condition = (:like column-name match-string) | (:like-insensitive column-name match-string) | (:starts-with column-name string)

from-clauses = (:from table-name on-expression*)
on-expression = simple-on-expression | using-columm-expression | via-column-table-expression 
simple-on-expression = (:on table-name sql-condition*)
using-columm-expression =  (:on table-name (:using column-name) sql-condition*)
via-column-table-expression =  (:on table-name (:via column-name table-name) sql-condition*)

order-by = (:order-by sql-expression+)

sql-expression = column-name | (sql-function sql-expression) | (+|- sql-expression+) | sql-condition 
sql-function  = symbol

sequence-expression = sql-literal+ | value-select-expression 
sql-literal = number | string | (:literal lisp-value literal-type) | (:date string)
literal-type = :date | :number | :string 
match-string = string ; use % as wild-card
value-select-expression = (:select select-options (column-name) from-clauses? where-clauses? group-by?)

defaults = (property-keyword value)
property-keyword = :defaults | :schema | :column-initial-cap | :column-case-sensitive | :db | :name-manager-class 
                   | :pretty | :trim | :empty-is-nil | :connection-string-function | :fully-qualify-columns 


