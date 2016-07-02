(library (odbc dbi)
  (export (rename (dbi-connect     connect     ))    
	  (rename (dbi-connect-raw connect-raw ))
	  (rename (dbi-disconnect  disconnect  ))
	  (rename (dbi-commit      commit      ))
	  (rename (dbi-rollback    rollback    ))
	  (rename (dbi-cursor      cursor      ))
	  (rename (dbi-query       query       ))
	  (rename (dbi-row-count   row-count   ))
	  (rename (dbi-ncols       number-of-columns       ))
	  (rename (dbi-col-defs    col-defs    ))
	  (rename (dbi-fetch-one   fetch-one   ))
	  (rename (map-sql-type    default-map-sql-type))
	  (rename (dbi-cursor-type-map-proc-set!
		                   cursor-type-map-proc-set!))
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
	    (mutable ncols)
	    (mutable coldef)
	    (mutable type-map-proc)))


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
		  (let* ([state (bv->string-with-length! state-bv 5)]
			 [nsi (dbi-sql-bv-parse-sshort native-state-int-bv)]
			 [l (dbi-sql-bv-parse-sshort message-l-bv)]
			 [mess (if (> l 0)
				   (bv->string-with-length! message-bv (- l 1))
				   "")])
		    (lp (+ 1 i) (format #f "~a ERROR: ~a: ~a:~a:~a\n" m i
				     state nsi mess)))
		  (make-message-condition m)))))))

  (define (call-check-res handle-type proc handle)
    (let ((res (proc))
	  (handle-int
	   (if (odbc-dbi-cursor? handle)
	       (odbc-dbi-connection-dbc-handle
		(odbc-dbi-cursor-connection handle))
	       handle)))
      (cond ((eq? res SQL_SUCCESS) #t)
	    ((eq? res SQL_SUCCESS_WITH_INFO) #t)
	    (#t (raise (condition (make-sql-error)
				  (sql-diag->conditions handle-type res handle-int)))))))
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
      (dbi-sql-bv-parse-sbigint out-bv)))

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
		       [out-dsn-l (dbi-sql-bv-parse-sshort out-dsn-l-bv)]
		       [out-dsn (bv->string-with-length!
				 out-dsn-bv
				 (if (>  out-dsn-l 0)
				     (- out-dsn-l 1)
				     0))])
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
      (make-odbc-dbi-cursor  stmt-handle connection -1 -1 #f #f)))
  
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


  (define dbi-get-all-col-defs
    (lambda  (cursor ncols)
      (if (< ncols  1)
	  #f
	  (let* ((stmt-handle (unbox (odbc-dbi-cursor-stmt-handle cursor)))
		 (coldefs (make-vector ncols)))
	    (letrec-syntax
		((get-cols-defs
		  (syntax-rules ()
		    ((_ (ic aname atype))
		     (let* ((out-buff-bv (make-bytevector 340 0))
			    (out-buff-len-bv (make-bytevector 4))
			    (out-num-attr-bv (make-bytevector 8))
			    (res-code (f-sql-col-attribute stmt-handle
							   (+ 1 ic)
							   aname
							   out-buff-bv
							   out-buff-len-bv
							   out-num-attr-bv))

			    (res-code-faked (if (eq? res-code SQL_NO_DATA)
						SQL_SUCCESS
						res-code))
			    (ret (call-check-res SQL_HANDLE_STMT
						 (lambda ()
						   res-code-faked)
						 cursor))

			    (out (if (eq? atype 'n)
				     (if (eq? res-code SQL_NO_DATA)
					 0
					 (dbi-sql-bv-parse-sshort
					  ;; FIXME: should use correct type
					  out-num-attr-bv))
				     (if (eq? res-code SQL_NO_DATA)
					 ""
					 (bv->string-with-length!
					  out-buff-bv
					  (dbi-sql-bv-parse-sshort
					   ;; FIXME: should use correct type
					   out-buff-len-bv ))))))
		       (cons out '()) ))
		    ((_ (a0 a1 a2) (ak a3 a4) ...)
		     (cons (car
			    (get-cols-defs (a0 a1 a2)))
		      (get-cols-defs (ak a3 a4) ...))))))
	      (let lp ((ic 0))
		(if (< ic ncols)
		    (begin 
		      (vector-set! coldefs ic
				   (get-cols-defs
				    (ic SQL_DESC_NAME 'c)
				    (ic SQL_DESC_LENGTH 'n)
				    (ic SQL_DESC_TYPE 'n)))
		      (lp (+ 1 ic)))
		    coldefs)))))))
  
  (define (dbi-query cursor sql)
    (let* ((s (refresh-stmt-handle cursor))
	   (stmt-handle (unbox (odbc-dbi-cursor-stmt-handle cursor)))
	   (sql-bv (string->bytevector sql (make-transcoder (utf-8-codec))))
	   (sql-len (bytevector-length sql-bv))
	   (res-code (f-sql-direct-execute stmt-handle sql-bv sql-len))
	   (res-code-faked (if (eq? res-code SQL_NO_DATA)
			       SQL_SUCCESS
			       res-code))
	   (ret (call-check-res SQL_HANDLE_STMT
				(lambda ()
				  res-code-faked)
				cursor))
	   (row-count (if (eq? res-code SQL_NO_DATA)
			  0
			  (dbi-get-row-count stmt-handle)))
	   (ncols (if (eq? res-code SQL_NO_DATA)
		      -1
		      (dbi-get-ncols stmt-handle))))
      (begin
	(odbc-dbi-cursor-row-count-set! cursor row-count)
	(odbc-dbi-cursor-ncols-set! cursor ncols)
	(odbc-dbi-cursor-coldef-set! cursor (dbi-get-all-col-defs cursor ncols))
	cursor)))

  (define (dbi-row-count cursor)
    (odbc-dbi-cursor-row-count cursor))
  (define (dbi-ncols cursor)
    (odbc-dbi-cursor-ncols cursor))
  (define (dbi-col-defs cursor)
    (odbc-dbi-cursor-coldef cursor))

  (define (dbi-get-data cursor col-idx type-convt)
    (let* ((stmt-handle (unbox (odbc-dbi-cursor-stmt-handle cursor)))
	   (col-def (vector-ref (dbi-col-defs cursor) col-idx ))
	   (col-len (cadr col-def))
	   (col-type (caddr col-def))
	   (col-type-covt ((if type-convt
			       type-convt
			       map-sql-type) col-type))
	   (target-type (car col-type-covt))
	   (target-len (max col-len
			    (if (cadr col-type-covt)
				(cadr col-type-covt)
				0)))
	   (target-cvt-proc (caddr col-type-covt))
	   (buffer-bv (make-bytevector (+ 4 target-len) 0))
	   (ind-bv (make-bytevector 8 0))
	   (res-code (f-sql-get-data stmt-handle
				     (+ 1 col-idx) target-type buffer-bv ind-bv))
	   (res-code-faked (if (eq? res-code SQL_NO_DATA)
			       SQL_SUCCESS
			       res-code))

	   (ret (call-check-res SQL_HANDLE_STMT
				(lambda () res-code-faked)
				cursor))

	   (data-len (dbi-sql-bv-parse-sbigint ind-bv))
	   (ret2 (call-check-res SQL_HANDLE_STMT
				 (lambda ()
				   (cond ((>= data-len 0) SQL_SUCCESS)
					 ((eq? data-len SQL_NULL_DATA) SQL_SUCCESS)
					 (#t SQL_ERROR)))
				 cursor)))
      (if (eq? data-len SQL_NULL_DATA)
	  '()
	  (if (not target-cvt-proc)
	      buffer-bv
	      (target-cvt-proc buffer-bv data-len)))))

  (define dbi-cursor-type-map-proc-set!
    odbc-dbi-cursor-type-map-proc-set!)
  
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
				      cursor))
		 (type-map (odbc-dbi-cursor-type-map-proc cursor)))
	    (if (or (eq? res-code SQL_NO_DATA) (< ncols 1))
		#f
		(let ((r (make-vector ncols)))
		  (let lp ((ci 0))
		    (if (< ci ncols)
			(begin
			  (vector-set!
			   r ci
			   (dbi-get-data cursor ci type-map))
			  (lp (+ 1 ci)))
			r)))))))))


