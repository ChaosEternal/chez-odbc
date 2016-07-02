(library (odbc base)
  (export f-sql-alloc-handle
	  f-sql-driver-connect
	  f-sql-disconnect
	  f-sql-end-transaction
	  f-sql-set-env-v3
	  f-sql-get-diag-rec
	  f-sql-direct-execute
	  f-sql-row-count
	  f-sql-ncols
	  f-sql-fetch
	  f-sql-get-data
	  f-sql-col-attribute
	  ;; f-make-odbc-cursor
	  ;; f-init-odbc-env
	  ;; f-init-odbc-hdl
	  ;; f-odbc-open
	  ;; f-odbc-execute
	  f-sql-free-handle
          SQL_DRIVER_NOPROMPT   
	  SQL_NULL_HANDLE       
	  SQL_HANDLE_ENV        
	  SQL_HANDLE_DBC        
	  SQL_HANDLE_STMT       
	  SQL_HANDLE_DESC       
	  SQL_HANDLE_SENV       
	  SQL_ATTR_ODBC_VERSION 
	  SQL_OV_ODBC3
	  SQL_COMMIT
	  SQL_ROLLBACK
	  SQL_SUCCESS
	  SQL_SUCCESS_WITH_INFO
	  SQL_NO_DATA
	  SQL_NULL_DATA
	  SQL_ERROR
	  SQL_DESC_NAME
	  SQL_DESC_LENGTH
	  SQL_DESC_TYPE
	  
	  )
  (import (chezscheme)
	  (rnrs bytevectors)
	  (odbc errors))

  (define _init
    (load-shared-object "libodbc.so"))

  (define SQL_DRIVER_NOPROMPT    0)
  (define SQL_NULL_HANDLE        0)

  (define SQL_HANDLE_ENV         1)
  (define SQL_HANDLE_DBC         2)
  (define SQL_HANDLE_STMT        3)
  (define SQL_HANDLE_DESC        4)
  (define SQL_HANDLE_SENV        5)

  (define SQL_ATTR_ODBC_VERSION  200)
  (define SQL_OV_ODBC3           3)

  (define SQL_COMMIT             0)
  (define SQL_ROLLBACK           1)

  (define SQL_SUCCESS            0)
  (define SQL_SUCCESS_WITH_INFO  1)
  (define SQL_NO_DATA            100)
  (define SQL_NULL_DATA          -1)
  (define SQL_ERROR              -1)

  
  (define SQL_DESC_NAME          1011)
  (define SQL_DESC_TYPE          1002)
  (define SQL_DESC_LENGTH        1003)
  
  (define-ftype
    f-cursor-space
    (struct (cols int)
      (ret int)
      (nrows int)
      (intdicator int)
      (res-space void*)))

  (define f-sql-alloc-handle
    (let* ([sql-alloc-handle
	    (foreign-procedure "SQLAllocHandle" (int void* u8*) int)])
      sql-alloc-handle))

  (define c-sql-set-env-attr
    (foreign-procedure  "SQLSetEnvAttr" (void* int void* int) int))
  

  (define (f-sql-set-env-v3 odbc-env-handler)
    (c-sql-set-env-attr odbc-env-handler SQL_ATTR_ODBC_VERSION  SQL_OV_ODBC3 0))
  
  
  (define f-sql-driver-connect
    (let ([c-sql-driver-connect
	   (foreign-procedure "SQLDriverConnect"
			      (void* void* string int u8* int u8* int) int)])
      (lambda (odbc-connect-ptr*
	       dsn
	       out-dsn-bv
	       out-dsn-len-bv)
	(let ([out-bv-len-alloced (bytevector-length out-dsn-bv)])
	  (c-sql-driver-connect
	   odbc-connect-ptr*
	   0
	   dsn
	   (string-length dsn)
	   out-dsn-bv
	   out-bv-len-alloced
	   out-dsn-len-bv
	   SQL_DRIVER_NOPROMPT
	   )))))

  (define f-sql-disconnect
    (foreign-procedure "SQLDisconnect" (void*) int))
  
  (define f-sql-end-transaction
    (foreign-procedure "SQLEndTran" (int void* int) int))
  
  (define f-sql-get-diag-rec
    (let ([c-sql-get-diag-rec
	   (foreign-procedure "SQLGetDiagRec"
			      (int void*
				   int u8*
				   u8* u8*
				   int u8*) int)])
      (lambda (handler-type
	       odbc-handler
	       rec-number
	       sql-state-bv
	       sql-native-error-int-bv
	       sql-message-bv
	       out-sql-message-len-bv)
	(let* ([sql-message-bv-len-alloced
		(bytevector-length sql-message-bv)])
	  (c-sql-get-diag-rec
	   handler-type
	   odbc-handler
	   rec-number
	   sql-state-bv
	   sql-native-error-int-bv
	   sql-message-bv
	   sql-message-bv-len-alloced
	   out-sql-message-len-bv
	   )))))
  (define f-sql-free-handle
    (foreign-procedure "SQLFreeHandle" (int void*) int))
  (define f-sql-direct-execute
    (foreign-procedure "SQLExecDirect" (void* u8* int) int))
  (define f-sql-row-count
    (foreign-procedure "SQLRowCount" (void* u8*) int))
  (define f-sql-ncols
    (foreign-procedure "SQLNumResultCols" (void* u8*) int))
  (define f-sql-fetch
    (foreign-procedure "SQLFetch" (void*) int))
  (define f-sql-get-data
    (let ((c-sql-get-data 
	   (foreign-procedure "SQLGetData" (void* int int u8* int u8*) int)))
      (lambda (stmt-handle col-idx to-type buffer-bv len-or-indicator-bv)
	(c-sql-get-data
	 stmt-handle
	 col-idx
	 to-type
	 buffer-bv
	 (bytevector-length buffer-bv)
	 len-or-indicator-bv))))

  (define f-sql-col-attribute
    (let ((c-sql-col-attribute
	   (foreign-procedure "SQLColAttribute" (void* int int u8* int u8* u8*) int)))
      (lambda (stmt-handle
	       col-idx
	       field-id
	       buffer-bv
	       len-out-bv
	       numerical-attr-out-bv)
	(c-sql-col-attribute
	 stmt-handle
	 col-idx
	 field-id
	 buffer-bv
	 (bytevector-length buffer-bv)
	 len-out-bv
	 numerical-attr-out-bv))))
  )
      
