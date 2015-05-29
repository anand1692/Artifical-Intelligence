(define (argsplit_true alist)
	(if (null? alist) '()
		(let ((item (car alist)))
			(if (not (list? item)) (append (list item) (argsplit_true (cdr alist)))
				(argsplit_true (cdr alist))))))

(define (argsplit_false alist)
	(if (null? alist) '()
		(let ((item (car alist)))
			(if (list? item) (append (cdr item) (argsplit_false (cdr alist)))
				(argsplit_false (cdr alist))))))	

(define (compare item blist)
	(cond ((null? blist) (list (append '(NOT) (list item))))
		  ((equal? item (car blist)) (cdr blist))
		  (else (append (list (car blist)) (compare item (cdr blist))))))

(define (compare_combine alist blist)
	(if (null? alist) blist
		(let ((clist (compare (car alist) blist)))
	;	(display clist) (newline)
			(compare_combine (cdr alist) clist))))

(define (check_contradiction alist blist)
	(if (and (not (list? (car alist))) (not (list? (car blist)))) #f 
		(if (or (not (equal? (length alist) 1)) (not (equal? (length blist) 1))) #f
			(if (list? (car alist)) 
				(equal? (cdr (car alist)) blist)
				(equal? (cdr (car blist)) alist)))))

(define (dedupe clist)
	(cond ((null? clist) '())
		  ((member (car clist) (cdr clist)) (dedupe (cdr clist)))
		  (else (cons (car clist) (dedupe (cdr clist))))))
	

(define (resolve alist blist)
	(if (null? alist) blist
		(if (null? blist) alist
			(if (or (equal? alist blist) (equal? alist (reverse blist))) #f
				(if (check_contradiction alist blist) '(CONTRADICTION) 
			  		(let* ((tlist1 (argsplit_true alist))
					 	(flist1 (argsplit_false alist))
				     	(tlist2 (argsplit_true blist))
				     	(flist2 (argsplit_false blist))
				     	(clist1 (compare_combine flist1 tlist2))
				     	(clist2 (compare_combine flist2 tlist1)))
				  ;   	(display clist1) (newline) (display clist2) (newline)
				     	(let ((final (dedupe (append clist2 clist1))))
							(if (null? final) #f
								(if (equal? (length final) (+ (length alist) (length blist))) #f
									(if (member final kb) #f
										final))))))))))

;In all the operators, just the list of arg is sent. i.e after removing the operator
(define (insert_not alist)
	(if (null? alist) '()
		(append (operator_not (list (car alist))) (operator_not (cdr alist)))))		  

(define (insert_not_bicond alist)
	(if (null? alist) '()
		(append (list (operator_not (car alist))) (list (operator_not (car (cdr alist)))))))		  

(define (operator_not alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (append '(NOT) (list (car alist)))) (operator_not (cdr alist))))
		  ((equal? (list (car (car alist))) '(NOT)) (append (cdr (car alist)) (operator_not (cdr alist))))
		  ((equal? (list (car (car alist))) '(OR)) (insert_not (operator_or (cdr (car alist)))))
 		  ((equal? (list (car (car alist))) '(IMPLIES)) (insert_not (operator_implies (cdr (car alist)))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (insert_not_bicond (operator_bicond (cdr (car alist)))))))

(define (operator_or alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (car alist)) (operator_or (cdr alist))))
		  ((equal? (list (car (car alist))) '(NOT)) (append (operator_not (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_not (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(OR)) (append (operator_or (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_or (cdr (cdr (car alist))))))
 		  ((equal? (list (car (car alist))) '(IMPLIES)) (append (operator_implies (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_implies (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (append (operator_bicond (cdr (car alist))) (operator_or (cdr alist)))))) ;(operator_bicond (cdr (car alist))))))

(define (operator_implies alist)
	(if (null? alist) '()
		(let ((var1 (operator_not (list (car alist)))))
			(cond ((not (list? (car (cdr alist)))) (append var1 (cdr alist)))
				  ((equal? (list (car (car (cdr alist)))) '(NOT)) (append var1 (operator_not (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(OR)) (append var1 (operator_or (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(IMPLIES)) (append var1 (operator_implies (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(BICONDITIONAL)) (append var1 (operator_bicond (cdr (car (cdr alist))))))))))

; Will call and operator for each var anded. Add each anded element to KB
(define (operator_and alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (list (car alist)))
		  ((equal? (list (car (car alist))) '(NOT)) (operator_not (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(OR)) (operator_or (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(IMPLIES)) (operator_implies (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (operator_bicond (cdr (car alist))))))
;
;Two elements returned which are result after simplyfying the bicond. Add both of them to KB. 
(define (operator_bicond alist)
	(if (null? alist) '()
		(let* ((var1 (operator_implies alist))
			   (var2 (operator_implies (reverse alist))))
			  (append (list var1) (list var2)))))	

(define kb '())

(define (dedupe_kb clist)
	(cond ((null? clist) '())
		  ((or (member (car clist) (cdr clist)) (member (reverse (car clist)) (cdr clist))) (dedupe_kb (cdr clist)))
		  (else (cons (car clist) (dedupe_kb (cdr clist))))))

(define (tell premise)
	(cond ((null? premise) '(EMPTY PREMISE))
		  ((not (list? premise)) (set! kb (append (list (list premise)) kb))); (display kb) (newline))
		  ((equal? (list (car premise)) '(NOT)) (set! kb (append (list (operator_not (cdr premise))) kb))); (display kb) (newline))
		  ((equal? (list (car premise)) '(OR)) (set! kb (append (list (operator_or (cdr premise))) kb))); (display kb) (newline))
		  ((equal? (list (car premise)) '(IMPLIES)) (set! kb (append (list (operator_implies (cdr premise))) kb))); (display kb) (newline))
		  ((equal? (list (car premise)) '(BICONDITIONAL)) (let ((bicond_var (operator_bicond (cdr premise))))
															   (set! kb (append (list (car bicond_var)) kb))
															   (set! kb (append (list (car (cdr bicond_var))) kb)))); (display kb) (newline))
		  ((equal? (list (car premise)) '(AND)) (let* ((and_var1 (operator_and (list (car (cdr premise)))))
													   (and_var2 (operator_and (cdr (cdr premise)))))
													   (set! kb (append (list and_var1) kb))
													   (set! kb (append (list and_var2) kb))))); (display kb) (newline))))
														   	
	(set! kb (dedupe_kb kb))
	(display 'OK) (newline))

(define (resolve_item item plist rlist complist)
	(if (null? plist) rlist
		(let ((resolution (resolve item (car plist))))
		;	 (display '(resolution :)) (display resolution) (newline)
			 (if (equal? resolution #f) (resolve_item item (cdr plist) rlist complist)
				(if (or (member resolution complist) (member (reverse resolution) complist)) (resolve_item item (cdr plist) rlist complist)
				 	(resolve_item item (cdr plist) (append (list resolution) rlist) complist))))))

; mlist =  main list with elements to resolve, plist = list of premises with whom to resolve, rlist = new resolution premises   
(define (resolve_prem mlist plist rlist complist)
	;(display '(mlist :)) (display mlist) (newline) (display '(plist :)) (display plist) (newline) (display '(rlist :)) (display rlist) (newline)
	(cond ((null? plist) 'UNKNOWN)
		  ((member '(CONTRADICTION) rlist) '(CONTRADICTION))
		  ((null? mlist) (resolve_prem (dedupe_kb (append complist plist)) rlist '() (dedupe_kb (append complist plist))))
		  (else
			(let ((new_rlist (resolve_item (car mlist) plist rlist (dedupe_kb (append complist plist)))))
				 (resolve_prem (cdr mlist) plist (dedupe_kb new_rlist) complist)))))  

(define (ask premise)
	(cond ((null? premise) '())
		  ((not (list? premise)) (let* ((negative (operator_not (list premise)))
			   						   (new_kb (append (list negative) kb))
			   						   (result (resolve_prem new_kb new_kb '() new_kb)))
			   						   (if (equal? result '(CONTRADICTION)) #t
				   						   'UNKNOWN)))
		  ((equal? (list (car premise)) '(BICONDITIONAL)) (let* ((negative (operator_not (list premise)))
																 (bicond (operator_bicond (cdr premise))))
															    (if (and (or (member (car bicond) kb) (member (reverse (car bicond)) kb))
																		(or (member (car (cdr bicond)) kb) (member (reverse (car (cdr bicond))) kb))) #t
																 (begin 
															     (let*	((new_kb1 (append (list (car negative)) kb))
																 		 (new_kb2 (append (list (car (cdr negative))) kb))
																 		 (result1 (resolve_prem new_kb1 new_kb1 '() new_kb1)))
																 		(if (equal? result1 '(CONTRADICTION)) (let ((result2 (resolve_prem new_kb2 new_kb2 '() new_kb2)))
																										   			(if (equal? result2 '(CONTRADICTION)) #t 'UNKNOWN))
																	 'UNKNOWN))))))
		  ((equal? (list (car premise)) '(AND)) (let* ((neg1 (operator_not (operator_and (list (car (cdr premise))))))
													   (neg2 (operator_not (operator_and (list (car (cdr (cdr premise)))))))
													   (new_kb1 (append (list neg1) kb))
													   (new_kb2 (append (list neg2) kb))
													   (result1 (resolve_prem new_kb1 new_kb1 '() new_kb1)))
                                                                 (if (equal? result1 '(CONTRADICTION)) (let ((result2 (resolve_prem new_kb2 new_kb2 '() new_kb2)))
                                                                                                           (if (equal? result2 '(CONTRADICTION)) #t 'UNKNOWN))
                                                                     'UNKNOWN)))
		 (else 
			(let* ((negative (operator_not (list premise)))
                   (new_kb (append (list negative) kb))
                   (result (resolve_prem new_kb new_kb '() new_kb)))
                   (if (equal? result '(CONTRADICTION)) #t
                       'UNKNOWN)))))
																 
			   





		 
