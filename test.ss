(import (prefix (odbc dbi) dbi:))

(define DSN (getenv "DSN"))

(define c (dbi:connect DSN))
(define cc (dbi:cursor c))

(dbi:query cc "select string_agg(relname||relnamespace, ',') from pg_class")
(format (current-output-port) "Number Rows: ~a\n" (dbi:ncols cc))
(display (vector-map car (dbi:col-defs cc)))
(newline)
(display (vector-map cadr (dbi:col-defs cc)))
(newline)
(display (vector-map caddr (dbi:col-defs cc)))
(newline)
(let lp ((d (dbi:fetch-one cc)))
  (if d
      (begin
	(display d)
	(newline)
	(lp (dbi:fetch-one cc))
	)))


