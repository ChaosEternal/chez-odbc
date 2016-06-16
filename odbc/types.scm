(library (odbc types)
  (export make-address-bv
	  bv-ref-address
	  bv->string-with-length!
	  dbi-sql-bv-parse-sshort
	  dbi-sql-bv-parse-ushort
	  dbi-sql-bv-parse-sint
	  dbi-sql-bv-parse-uint
	  dbi-sql-bv-parse-sbigint
	  dbi-sql-bv-parse-ubigint
	  dbi-sql-bv-parse-len
	  dbi-sql-bv-parse-ulen

	  sql-type-convt-map

	  SQL_VARCHAR
	  
	  SQL_CODE_DATE
	  SQL_CODE_TIME
	  SQL_CODE_TIMESTAMP
	  SQL_CODE_YEAR
	  SQL_CODE_MONTH
	  SQL_CODE_DAY
	  SQL_CODE_HOUR
	  SQL_CODE_MINUTE
	  SQL_CODE_SECOND
	  SQL_CODE_YEAR_TO_MONTH
	  SQL_CODE_DAY_TO_HOUR
	  SQL_CODE_DAY_TO_MINUTE
	  SQL_CODE_DAY_TO_SECOND
	  SQL_CODE_HOUR_TO_MINUTE
	  SQL_CODE_HOUR_TO_SECOND
	  SQL_CODE_MINUTE_TO_SECOND
	  SQL_CHAR
	  SQL_NUMERIC
	  SQL_INTEGER
	  SQL_SMALLINT
	  SQL_REAL
	  SQL_DOUBLE
	  SQL_TYPE_DATE
	  SQL_TYPE_TIME
	  SQL_TYPE_TIMESTAMP
	  SQL_DATE
	  SQL_TIME
	  SQL_TIMESTAMP
	  SQL_BINARY
	  SQL_BIGINT
	  SQL_TINYINT
	  SQL_BIT
	  SQL_GUID
	  SQL_INTERVAL_YEAR
	  SQL_INTERVAL_MONTH
	  SQL_INTERVAL_DAY
	  SQL_INTERVAL_HOUR
	  SQL_INTERVAL_MINUTE
	  SQL_INTERVAL_SECOND
	  SQL_INTERVAL_YEAR_TO_MONTH
	  SQL_INTERVAL_DAY_TO_HOUR
	  SQL_INTERVAL_DAY_TO_MINUTE
	  SQL_INTERVAL_DAY_TO_SECOND
	  SQL_INTERVAL_HOUR_TO_MINUTE
	  SQL_INTERVAL_HOUR_TO_SECOND
	  SQL_INTERVAL_MINUTE_TO_SECOND
	  SQL_SIGNED_OFFSET
	  SQL_UNSIGNED_OFFSET
	  SQL_DEFAULT
	  SQL_WCHAR
	  SQL_C_CHAR
	  SQL_C_LONG
	  SQL_C_SHORT
	  SQL_C_FLOAT
	  SQL_C_DOUBLE
	  SQL_C_NUMERIC
	  SQL_C_DEFAULT
	  SQL_C_DATE
	  SQL_C_TIME
	  SQL_C_TIMESTAMP
	  SQL_C_TYPE_DATE
	  SQL_C_TYPE_TIME
	  SQL_C_TYPE_TIMESTAMP
	  SQL_C_INTERVAL_YEAR
	  SQL_C_INTERVAL_MONTH
	  SQL_C_INTERVAL_DAY
	  SQL_C_INTERVAL_HOUR
	  SQL_C_INTERVAL_MINUTE
	  SQL_C_INTERVAL_SECOND
	  SQL_C_INTERVAL_YEAR_TO_MONTH
	  SQL_C_INTERVAL_DAY_TO_HOUR
	  SQL_C_INTERVAL_DAY_TO_MINUTE
	  SQL_C_INTERVAL_DAY_TO_SECOND
	  SQL_C_INTERVAL_HOUR_TO_MINUTE
	  SQL_C_INTERVAL_HOUR_TO_SECOND
	  SQL_C_INTERVAL_MINUTE_TO_SECOND
	  SQL_C_BINARY
	  SQL_C_BIT
	  SQL_C_SBIGINT
	  SQL_C_UBIGINT
	  SQL_C_TINYINT
	  SQL_C_SLONG
	  SQL_C_SSHORT
	  SQL_C_STINYINT
	  SQL_C_ULONG
	  SQL_C_USHORT
	  SQL_C_UTINYINT
	  SQL_C_BOOKMARK
	  SQL_C_GUID
	  SQL_C_VARBOOKMARK
	  SQL_C_WCHAR
	  SQL_C_TCHAR
	  )
  (import (odbc base)
	  (chezscheme))

  (define SQL_CODE_DATE 1)
  (define SQL_CODE_TIME 2)
  (define SQL_CODE_TIMESTAMP 3)
  (define SQL_CODE_YEAR 1)
  (define SQL_CODE_MONTH 2)
  (define SQL_CODE_DAY 3)
  (define SQL_CODE_HOUR 4)
  (define SQL_CODE_MINUTE 5)
  (define SQL_CODE_SECOND 6)
  (define SQL_CODE_YEAR_TO_MONTH 7)
  (define SQL_CODE_DAY_TO_HOUR 8)
  (define SQL_CODE_DAY_TO_MINUTE 9)
  (define SQL_CODE_DAY_TO_SECOND 10)
  (define SQL_CODE_HOUR_TO_MINUTE 11)
  (define SQL_CODE_HOUR_TO_SECOND 12)
  (define SQL_CODE_MINUTE_TO_SECOND 13)
  (define SQL_DEFAULT 99)
  (define SQL_CHAR 1)
  (define SQL_NUMERIC 2)
  (define SQL_INTEGER 4)
  (define SQL_SMALLINT 5)
  (define SQL_REAL 7)
  (define SQL_DOUBLE 8)

  (define SQL_VARCHAR 12)
  
  (define SQL_TYPE_DATE 91)
  (define SQL_TYPE_TIME 92)
  (define SQL_TYPE_TIMESTAMP 93)
  (define SQL_DATE 9)
  (define SQL_TIME 10)
  (define SQL_TIMESTAMP 11)
  (define SQL_BINARY -2)
  (define SQL_BIGINT -5)
  (define SQL_TINYINT -6)
  (define SQL_BIT -7)
  (define SQL_GUID -11)
  (define SQL_INTERVAL_YEAR (+ 100  SQL_CODE_YEAR))
  (define SQL_INTERVAL_MONTH (+ 100  SQL_CODE_MONTH))
  (define SQL_INTERVAL_DAY (+ 100  SQL_CODE_DAY))
  (define SQL_INTERVAL_HOUR (+ 100  SQL_CODE_HOUR))
  (define SQL_INTERVAL_MINUTE (+ 100  SQL_CODE_MINUTE))
  (define SQL_INTERVAL_SECOND (+ 100  SQL_CODE_SECOND))
  (define SQL_INTERVAL_YEAR_TO_MONTH (+ 100  SQL_CODE_YEAR_TO_MONTH))
  (define SQL_INTERVAL_DAY_TO_HOUR (+ 100  SQL_CODE_DAY_TO_HOUR))
  (define SQL_INTERVAL_DAY_TO_MINUTE (+ 100  SQL_CODE_DAY_TO_MINUTE))
  (define SQL_INTERVAL_DAY_TO_SECOND (+ 100  SQL_CODE_DAY_TO_SECOND))
  (define SQL_INTERVAL_HOUR_TO_MINUTE (+ 100  SQL_CODE_HOUR_TO_MINUTE))
  (define SQL_INTERVAL_HOUR_TO_SECOND (+ 100  SQL_CODE_HOUR_TO_SECOND))
  (define SQL_INTERVAL_MINUTE_TO_SECOND (+ 100  SQL_CODE_MINUTE_TO_SECOND))
  (define SQL_SIGNED_OFFSET -20)
  (define SQL_UNSIGNED_OFFSET -22)
  (define SQL_WCHAR -8)
  (define SQL_C_CHAR SQL_CHAR)
  (define SQL_C_LONG SQL_INTEGER)
  (define SQL_C_SHORT SQL_SMALLINT)
  (define SQL_C_FLOAT SQL_REAL)
  (define SQL_C_DOUBLE SQL_DOUBLE)
  (define SQL_C_NUMERIC SQL_NUMERIC)
  (define SQL_C_DEFAULT 99)
  (define SQL_C_DATE SQL_DATE)
  (define SQL_C_TIME SQL_TIME)
  (define SQL_C_TIMESTAMP SQL_TIMESTAMP)
  (define SQL_C_TYPE_DATE SQL_TYPE_DATE)
  (define SQL_C_TYPE_TIME SQL_TYPE_TIME)
  (define SQL_C_TYPE_TIMESTAMP SQL_TYPE_TIMESTAMP)
  (define SQL_C_INTERVAL_YEAR SQL_INTERVAL_YEAR)
  (define SQL_C_INTERVAL_MONTH SQL_INTERVAL_MONTH)
  (define SQL_C_INTERVAL_DAY SQL_INTERVAL_DAY)
  (define SQL_C_INTERVAL_HOUR SQL_INTERVAL_HOUR)
  (define SQL_C_INTERVAL_MINUTE SQL_INTERVAL_MINUTE)
  (define SQL_C_INTERVAL_SECOND SQL_INTERVAL_SECOND)
  (define SQL_C_INTERVAL_YEAR_TO_MONTH SQL_INTERVAL_YEAR_TO_MONTH)
  (define SQL_C_INTERVAL_DAY_TO_HOUR SQL_INTERVAL_DAY_TO_HOUR)
  (define SQL_C_INTERVAL_DAY_TO_MINUTE SQL_INTERVAL_DAY_TO_MINUTE)
  (define SQL_C_INTERVAL_DAY_TO_SECOND SQL_INTERVAL_DAY_TO_SECOND)
  (define SQL_C_INTERVAL_HOUR_TO_MINUTE SQL_INTERVAL_HOUR_TO_MINUTE)
  (define SQL_C_INTERVAL_HOUR_TO_SECOND SQL_INTERVAL_HOUR_TO_SECOND)
  (define SQL_C_INTERVAL_MINUTE_TO_SECOND SQL_INTERVAL_MINUTE_TO_SECOND)
  (define SQL_C_BINARY SQL_BINARY)
  (define SQL_C_BIT SQL_BIT)
  (define SQL_C_SBIGINT (+ SQL_BIGINT SQL_SIGNED_OFFSET))
  (define SQL_C_UBIGINT (+ SQL_BIGINT SQL_UNSIGNED_OFFSET))
  (define SQL_C_TINYINT SQL_TINYINT)
  (define SQL_C_SLONG (+ SQL_C_LONG SQL_SIGNED_OFFSET))
  (define SQL_C_SSHORT (+ SQL_C_SHORT SQL_SIGNED_OFFSET))
  (define SQL_C_STINYINT (+ SQL_TINYINT SQL_SIGNED_OFFSET))
  (define SQL_C_ULONG (+ SQL_C_LONG SQL_UNSIGNED_OFFSET))
  (define SQL_C_USHORT (+ SQL_C_SHORT SQL_UNSIGNED_OFFSET))
  (define SQL_C_UTINYINT (+ SQL_TINYINT SQL_UNSIGNED_OFFSET))
  (define SQL_C_BOOKMARK SQL_C_UBIGINT)
  (define SQL_C_GUID SQL_GUID)
  (define SQL_C_VARBOOKMARK SQL_C_BINARY)
  (define SQL_C_WCHAR SQL_WCHAR)
  (define SQL_C_TCHAR SQL_C_CHAR)


  (define (make-address-bv)
    (make-bytevector (ftype-sizeof void*) 0))
  
  (define bv-ref-address
    (let* ((sp (ftype-sizeof void*))
	   (bv-ref (if (> sp 4)
		       bytevector-s64-native-ref
		       bytevector-s32-native-ref)))
      (lambda (bv)
	(bv-ref bv 0))))

  (define-syntax  bv->string-with-length!
    (syntax-rules ()
      ((_ bv len)
       (bytevector->string
	(bytevector-truncate! bv len)
	(make-transcoder (utf-8-codec))))))

  (define-syntax bv-parse-some
    (syntax-rules ()
      ((_ x y)
       (define-syntax x
	 (syntax-rules ()
	   ((_ xx)
	    (x xx 0))
	   ((_ xx n)
	    (y xx n)))))))

  (define-syntax bulk-def-parse-some
    (syntax-rules ()
      ((_ (n t))
       (bv-parse-some n t))
      ((_ (n t) (n1 t1) ...)
       (begin 
	 (bv-parse-some n t)
	 (bulk-def-parse-some (n1 t1) ...)))))

  (bulk-def-parse-some
   (dbi-sql-bv-parse-sshort   bytevector-s16-native-ref) 
   (dbi-sql-bv-parse-ushort   bytevector-u16-native-ref) 
   (dbi-sql-bv-parse-sint     bytevector-s32-native-ref)
   (dbi-sql-bv-parse-uint     bytevector-u32-native-ref)
   (dbi-sql-bv-parse-sbigint  bytevector-s64-native-ref)
   (dbi-sql-bv-parse-ubigint  bytevector-u64-native-ref)
   (dbi-sql-bv-parse-len      bytevector-s64-native-ref)
   (dbi-sql-bv-parse-ulen     bytevector-u64-native-ref)
   (dbi-sql-bv-parse-float    bytevector-ieee-single-native-ref)
   (dbi-sql-bv-parse-double   bytevector-ieee-double-native-ref)
   )
  ;; the defined syntaxes are equal as following
  ;; (define-syntax dbi-sql-bv-parse-sshort
  ;;   (syntax-rules ()
  ;;     ((_ x)
  ;;      (dbi-sql-bv-parse-sshort x 0))
  ;;     ((_ x n)
  ;;      (bytevector-s16-native-ref x n))))

  (define sql-type-convt-map (make-eq-hashtable))

  ;; hash-entry is (SQL_C_[TYPE] length convert-proc)
  ;; convert-proc is (lambda (bv length) ...)
  (for-each (lambda (T)
	      (hashtable-set! sql-type-convt-map
			      T
			      (list
			       SQL_C_CHAR #f
			       (lambda (bv length)
				 (bv->string-with-length! bv length)))))
	    (list SQL_CHAR
		  SQL_NUMERIC
		  SQL_TYPE_DATE
		  SQL_TYPE_TIME
		  SQL_TYPE_TIMESTAMP
		  SQL_DATE
		  SQL_TIME
		  SQL_TIMESTAMP
		  SQL_BINARY
		  SQL_BIT
		  SQL_GUID
		  SQL_WCHAR
		  SQL_DEFAULT))

  (hashtable-set! sql-type-convt-map
		  SQL_INTEGER 
		  (list
		   SQL_C_LONG (ftype-sizeof int)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-sint bv) )))

  (hashtable-set! sql-type-convt-map
		  SQL_SMALLINT 
		  (list
		   SQL_C_SHORT (ftype-sizeof short)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-sshort bv) )))

  (hashtable-set! sql-type-convt-map
		  SQL_REAL 
		  (list
		   SQL_C_FLOAT (ftype-sizeof short)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-float bv) )))

  (hashtable-set! sql-type-convt-map
		  SQL_DOUBLE 
		  (list
		   SQL_C_FLOAT (ftype-sizeof short)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-double bv) )))
  ;; DATE;TIME;TIMESTAMP
  ;; (hashtable-set! sql-type-convt-map
  ;; 		  SQL_TYPE_DATE 
  ;; 		  (list
  ;; 		   SQL_C_CHAR #f
  ;; 		   (lambda (bv length)
  ;; 		     (bv->string-with-length! bv length) )))

  (hashtable-set! sql-type-convt-map
		  SQL_BINARY
		  (list
		   SQL_C_BINARY #f #f))

  (hashtable-set! sql-type-convt-map
		  SQL_BIGINT
		  (list
		   SQL_C_SBIGINT (ftype-sizeof long)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-sbigint bv) )))

  (hashtable-set! sql-type-convt-map
		  SQL_TINYINT
		  (list
		   SQL_C_SHORT (ftype-sizeof short)
		   (lambda (bv length)
		     (dbi-sql-bv-parse-sshort bv) )))


  )
