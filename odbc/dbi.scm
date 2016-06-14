(library (odbc dbi)
  (export (rename (dbi-connect     connect     ))    
	  (rename (dbi-connect-raw connect-raw ))
	  (rename (dbi-disconnect  disconnect  ))
	  (rename (dbi-commit      commit      ))
	  (rename (dbi-rollback    rollback    ))
	  (rename (dbi-cursor      cursor      ))
	  (rename (dbi-query       query       ))
	  (rename (dbi-row-count   row-count   ))
	  (rename (dbi-ncols       ncols       ))
	  (rename (dbi-fetch-one   fetch-one   ))
	  ;; dbi-status
	  ;; dbi-get-col-spec
	  )
  (import (chezscheme)
	  (odbc base)
	  (odbc types)
	  (odbc errors))

  ;; data structures
  (define-record-type odbc-dbi-connection
    (fields env-handle
	    dbc-handle
	    out-dsn))
  (define-record-type odbc-dbi-cursor
    (fields (mutable stmt-handle)
	    connection
	    (mutable row-count)
	    (mutable ncols)))

  
  ;; exceptions processors
  (define (sql-diag->conditions handle-type res-code handle)
    (if (eq? handle 0)
	(make-message-condition (format #f "Code: ~a ~a\n" handle-type res-code))
	(let* ([message-bv (make-bytevector 128 0)]
		  [state-bv (make-bytevector 6 0)]
		  [native-state-int-bv (make-bytevector 4 0)]
		  [message-l-bv (make-bytevector 4 0)])
	  (let lp ((i 1)
		   (m (format #f "Code: ~a ~a\n" handle-type res-code)))
	       (let ((r (f-sql-get-diag-rec 2
					    handle
					    i
					    state-bv
					    native-state-int-bv
					    message-bv
					    message-l-bv)))
		 (if (eq? r SQL_SUCCESS)
		     (let* ([state (bytevector->string
				    (bytevector-truncate! state-bv 5)
				    (make-transcoder (utf-8-codec)))]
			    [nsi (bytevector-s16-native-ref native-state-int-bv 0)]
			    [l (bytevector-s16-native-ref message-l-bv 0)]
			    [mess (bytevector->string
				(bytevector-truncate! message-bv (- l 1))
				(make-transcoder (utf-8-codec)))])
		       (lp i (format #f "~a ERROR: ~a: ~a:~a:~a\n" m i
				     state nsi mess)))
		     (make-message-condition m)))))))

  (define (call-check-res handle-type proc handle)
    (let ((res (proc)))
      (cond ((eq? res SQL_SUCCESS) #t)
	    ((eq? res SQL_SUCCESS_WITH_INFO) #t)
	    (#t (raise (condition (make-sql-error)
				  (sql-diag->conditions handle-type res handle)))))))
  ;; low-level interfacesn
  (define (dbi-alloc-something what in-handle)
    (let* ((out-bv (make-address-bv))
	   (res (call-check-res
		 what (lambda ()
				  (f-sql-alloc-handle
				   what
				   in-handle
				   out-bv)) in-handle)))
      (bv-ref-address out-bv)))

  (define (dbi-alloc-env)
    (dbi-alloc-something SQL_HANDLE_ENV SQL_NULL_HANDLE))
  (define (dbi-alloc-dbc env-handle)
    (dbi-alloc-something SQL_HANDLE_DBC env-handle))
  (define (dbi-alloc-stmt dbc-handle)
    (dbi-alloc-something SQL_HANDLE_STMT dbc-handle))

  (define (dbi-call-and-get-long proc what in-handle)
    (let* ((out-bv (make-bytevector 8 0))
	   (res (call-check-res
		 what (lambda ()
			(proc in-handle out-bv)) in-handle)))
      (bytevector-s64-native-ref out-bv 0)))

  (define (dbi-get-row-count stmt-handle)
    (dbi-call-and-get-long f-sql-row-count SQL_HANDLE_STMT stmt-handle))
  (define (dbi-get-ncols stmt-handle)
    (dbi-call-and-get-long f-sql-ncols SQL_HANDLE_STMT stmt-handle))
  
  
  ;; connection helper procedure
  (define (dbi-do-connect dbc-handle dsn out-dsn-bv out-dsn-bv-l)
    (call-check-res SQL_HANDLE_DBC (lambda ()
				     (f-sql-driver-connect
				      dbc-handle
				      dsn
				      out-dsn-bv
				      out-dsn-bv-l)) dbc-handle))
  ;; make connection
  ;; input: raw dsn: DSN=xxx;UID=xxx;...
  ;; output: connection 
  (define (dbi-connect-raw dsn)
    (let* ([env-handle (dbi-alloc-env)]
	   [setenvr (f-sql-set-env-v3 env-handle)])
      (with-exception-handler 
	  (lambda (x)
	    (begin 
	      (f-sql-free-handle SQL_HANDLE_ENV env-handle)
	      (raise x)))
	(lambda ()
	  (let* ([dbc-handle (dbi-alloc-dbc env-handle)])
	    (with-exception-handler
		(lambda (x)
		  (begin (f-sql-free-handle SQL_HANDLE_DBC dbc-handle)
			 (raise x)))
	      (lambda ()
		(let* ([out-dsn-bv (make-bytevector 1024 0)]
		       [out-dsn-l-bv (make-bytevector 4 0)]
		       [res (dbi-do-connect dbc-handle dsn out-dsn-bv out-dsn-l-bv)]
		       [out-dsn-l (bytevector-s16-native-ref out-dsn-l-bv 0)]
		       [out-dsn (bytevector->string (bytevector-truncate!
						     out-dsn-bv
						     (if (>  out-dsn-l 0)
							 (- out-dsn-l 1)
							 0))
						    (make-transcoder (utf-8-codec)))])
		  (make-odbc-dbi-connection env-handle dbc-handle
					    out-dsn)))))))))
  ;; input: dsn_without_"DSN=" uid [pwd]
  ;;        dns_with_"DSN="
  ;; output: connection
  (define dbi-connect
    (case-lambda
      [(dsn uid pwd) (dbi-connect (format #f "DSN=~a;UID=~a;PWD=~a" dsn uid pwd))]
      [(dsn uid) (dbi-connect (format #f "DSN=~a;UID=~a" dsn uid))]
      [(dsn) (dbi-connect-raw dsn)]))
  
  (define (dbi-disconnect connection)
    (let ([dbc-handle (odbc-dbi-connection-dbc-handle connection)]
	  [env-handle (odbc-dbi-connection-env-handle connection)])
      (begin
	(f-sql-disconnect dbc-handle)
	(f-sql-free-handle
	 SQL_HANDLE_DBC dbc-handle)
	(f-sql-free-handle
	 SQL_HANDLE_ENV env-handle)
	(void))))
  
  (define (dbi-commit connection)
    (let ([dbc-handle (odbc-dbi-connection-dbc-handle connection)])
      (f-sql-end-transaction SQL_HANDLE_DBC dbc-handle SQL_COMMIT)))
  
  (define (dbi-rollback connection)
    (let ([dbc-handle (odbc-dbi-connection-dbc-handle connection)])
      (f-sql-end-transaction SQL_HANDLE_DBC dbc-handle SQL_ROLLBACK)))

  (define (dbi-cursor connection)
    (let ((stmt-handle (box -1)))
      (make-odbc-dbi-cursor  stmt-handle connection -1 -1)))
  
  (define refresh-stmt-handle
    (letrec* ((stmt-guardian (make-guardian))
	      (p 
	       (lambda (cursor)
		 (let* ([dbc-handle (odbc-dbi-connection-dbc-handle
				     (odbc-dbi-cursor-connection cursor))]
			[stmt-handle (box (dbi-alloc-stmt dbc-handle))]
			[r (stmt-guardian stmt-handle)])
		   (begin
		     (odbc-dbi-cursor-stmt-handle-set! cursor stmt-handle)
		     stmt-handle)
		   )))
	      (free-handle (lambda ()
			     (collect)
			     (let lp ()
			       (let ((b (stmt-guardian)))
				 (when (box? b)
				   (f-sql-free-handle SQL_HANDLE_STMT (unbox b))
				   (lp)))))))
      (begin (collect-request-handler free-handle)
	     p)))
  
  (define (dbi-query cursor sql)
    (let* ((s (refresh-stmt-handle cursor))
	   (stmt-handle (unbox (odbc-dbi-cursor-stmt-handle cursor)))
	   (sql-len (string-length sql))
	   (res-code (f-sql-direct-execute stmt-handle sql sql-len))
	   (res-code-faked (if (eq? res-code SQL_NO_DATA)
			       SQL_SUCCESS
			       res-code))
	   (ret (call-check-res SQL_HANDLE_STMT
				(lambda ()
				  res-code-faked)
				stmt-handle))
	   (row-count (if (eq? res-code SQL_NO_DATA)
			  0
			  (dbi-get-row-count stmt-handle)))
	   (ncols (if (eq? res-code SQL_NO_DATA)
		      -1
		      (dbi-get-ncols stmt-handle))))
      (begin
	(odbc-dbi-cursor-row-count-set! cursor row-count)
	(odbc-dbi-cursor-ncols-set! cursor ncols))))

  (define (dbi-row-count cursor)
    (odbc-dbi-cursor-row-count cursor))
  (define (dbi-ncols cursor)
    (odbc-dbi-cursor-ncols cursor))
  
  (define (dbi-get-data stmt-handle col-idx to-type)
    (let* ((buffer-bv (make-bytevector 1024 0))
	   (ind-bv (make-bytevector 8 0))
	   (res-code (f-sql-get-data stmt-handle col-idx SQL_C_CHAR buffer-bv ind-bv))
	   (res-code-faked (if (eq? res-code SQL_NO_DATA)
			      SQL_SUCCESS
			      res-code))

	   (ret (call-check-res SQL_HANDLE_STMT
				(lambda () res-code-faked)
				stmt-handle))

	   (data-len (bytevector-s64-native-ref ind-bv 0))
	   (ret2 (call-check-res SQL_HANDLE_STMT
				 (lambda ()
				   (cond ((>= data-len 0) SQL_SUCCESS)
					 ((eq? data-len SQL_NO_DATA) SQL_SUCCESS)
					 (#t SQL_ERROR)))
				 stmt-handle)))
      (if (eq? data-len SQL_NO_DATA)
	  '()
	  (bv->string-length! buffer-bv data-len))))
  
  (define (dbi-fetch-one cursor)
    (let* ((stmt-handle (unbox (odbc-dbi-cursor-stmt-handle cursor))))
      (if (< stmt-handle 0)
	  #f
	  (let* ((res-code (f-sql-fetch stmt-handle))
		 (ncols (odbc-dbi-cursor-ncols cursor))
		 (res-code-faked (if (eq? res-code SQL_NO_DATA)
				     SQL_SUCCESS
				     res-code))
		 (ret (call-check-res SQL_HANDLE_STMT
				      (lambda () res-code-faked)
				      stmt-handle)))
	    (if (or (eq? res-code SQL_NO_DATA) (< ncols 1))
		#f
		(let ((r (make-vector ncols)))
		  (let lp ((ci 0))
		    (if (< ci ncols)
			(begin
			  (vector-set!
			   r ci
			   (dbi-get-data stmt-handle (+ 1 ci) SQL_C_CHAR))
			  (lp (+ 1 ci)))
			r))))))))
  )
  
