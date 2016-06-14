(import (odbc dbi))

(define DSN (getenv "DSN"))

(define c (dbi-connect DSN))
(define cc (dbi-cursor c))

(dbi-query cc "select now()-'2001-01-01' limit 1")
(display  (dbi-fetch-one cc))


