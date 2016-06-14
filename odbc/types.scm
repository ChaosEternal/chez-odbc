(library (odbc types)
  (export make-address-bv
	  bv-ref-address
	  bv->string-length!
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
  (define (bv->string-length! bv len)
    (bytevector->string
     (bytevector-truncate! bv len)
     (make-transcoder (utf-8-codec))))

  (define-syntax dbi-sql-bv-parse-sshort
    (syntax-rules ()
      ((_ x)
       (bytevector-s16-native-ref x 0))))
  
  (define-syntax dbi-sql-bv-parse-ushort
    (syntax-rules ()
      ((_ x)
       (bytevector-u16-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-sint
    (syntax-rules ()
      ((_ x)
       (bytevector-s32-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-uint
    (syntax-rules ()
      ((_ x)
       (bytevector-u32-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-sbigint
    (syntax-rules ()
      ((_ x)
       (bytevector-s64-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-ubigint
    (syntax-rules ()
      ((_ x)
       (bytevector-u64-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-len
    (syntax-rules ()
      ((_ x)
       (bytevector-s64-native-ref x 0))))

  (define-syntax dbi-sql-bv-parse-ulen
    (syntax-rules ()
      ((_ x)
       (bytevector-u64-native-ref x 0))))

  

)
