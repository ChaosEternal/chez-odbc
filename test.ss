(import (prefix (odbc dbi) dbi:)
	(odbc types))

(define DSN (getenv "DSN"))

(define c (dbi:connect DSN))
(define cc (dbi:cursor c))

(for-each 
 (lambda (qx)
   (dbi:query cc qx)
   (format (current-output-port) "Number Rows: ~a\n" (dbi:number-of-columns cc))
   (display (vector-map car (dbi:col-defs cc)))
   (newline)
   (display (vector-map cadr (dbi:col-defs cc)))
   (newline)
   (display (vector-map caddr (dbi:col-defs cc)))
   (newline)
   (let lp ((d (dbi:fetch-one cc)))
     (if d
	 (begin
	   (format (current-output-port)
		   "~a\n" ;;(pretty-print)
		   d)
	   (lp (dbi:fetch-one cc))
	   ))))
 (list  "select * from pg_class"
	"select now()"
	"select * from pg_attribute"
	"select 'abc'::bytea"
	"select 'abcadasdadasdlakdjalkdjaldjadsadsajdlajdlajdslajdlajdlajdlajdlajdlajdlajdlaksjdadjaljdajdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddadjsklllllllllllllllllllllllllllllllllllllllllllllllllllllla'::bytea")
 )


