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
	  
	  SQL_CHAR    
	  SQL_NUMERIC 
	  SQL_DECIMAL 
	  SQL_INTEGER 
	  SQL_SMALLINT
	  SQL_FLOAT   
	  SQL_REAL    
	  SQL_DOUBLE  
	  SQL_C_CHAR  
	  SQL_C_LONG  
	  SQL_C_SHORT 
	  SQL_C_FLOAT 
	  SQL_C_DOUBLE
	  )
  (import (odbc base)
	  (chezscheme))
  
  (define SQL_CHAR            1)
  (define SQL_NUMERIC         2)
  (define SQL_DECIMAL         3)
  (define SQL_INTEGER         4)
  (define SQL_SMALLINT        5)
  (define SQL_FLOAT           6)
  (define SQL_REAL            7)
  (define SQL_DOUBLE          8)

  (define SQL_C_CHAR    SQL_CHAR     )
  (define SQL_C_LONG    SQL_INTEGER  )
  (define SQL_C_SHORT   SQL_SMALLINT )
  (define SQL_C_FLOAT   SQL_REAL     )
  (define SQL_C_DOUBLE  SQL_DOUBLE   )


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
      ((_ (n t) rest)
       (begin 
	 (bv-parse-some n t)
	 (bulk-def-parse-some rest)))
      ((_ (n t))
       (bv-parse-some n t))))

  (bulk-def-parse-some
   (dbi-sql-bv-parse-sshort   bytevector-s16-native-ref) 
   (dbi-sql-bv-parse-ushort   bytevector-u16-native-ref)
   (dbi-sql-bv-parse-sint     bytevector-s32-native-ref)
   (dbi-sql-bv-parse-uint     bytevector-u32-native-ref)
   (dbi-sql-bv-parse-sbigint  bytevector-s64-native-ref)
   (dbi-sql-bv-parse-ubigint  bytevector-u64-native-ref)
   (dbi-sql-bv-parse-len      bytevector-s64-native-ref)
   (dbi-sql-bv-parse-ulen     bytevector-u64-native-ref)
   )
  ;; the defined syntaxes are equal as following
  ;; (define-syntax dbi-sql-bv-parse-sshort
  ;;   (syntax-rules ()
  ;;     ((_ x)
  ;;      (dbi-sql-bv-parse-sshort x 0))
  ;;     ((_ x n)
  ;;      (bytevector-s16-native-ref x n))))



)
