(library (odbc errors)
  (export &sql-info
	  make-sql-info
	  sql-info?
	  &sql-error
	  make-sql-error
	  sql-error?)
  (import (chezscheme))
  (define-condition-type &sql-info &message make-sql-info sql-info?)
  (define-condition-type &sql-error &error make-sql-error sql-error?)
  
  )
