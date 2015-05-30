;; Assignment 6 - Automated Inference - Theorem Prover : Advanced
; The program implements a Propositional Logic Solver that accepts arbitrary propositional statements,
; converts them into CNF form and then performs resolution to answer propositional logic questions. The program 
; supports unary and binary operations only. The operands supported are : NOT, OR, AND, IMPLIES, BICONDITIONAL.
;
; The program returns #t if the negation of the premise resolves in a contradiction or UNKNOWN otherwise. 
; The program accepts the argument of the following forms :
; 	1. (OR a b), (OR a (OR b c)), (OR (OR a b) (OR b d)) - any unary/binary combination with any operator
; 	2. If single literal, it is passed without a list. Eg: (ask 'hasDog) and not (ask '(hasDog))
; 	3. Other inputs as described in the description
;
; Created by Anand Goyal © May 2015

; Function to split the argument list and extract the true literals, that is without a NOT
(define (argsplit_true alist)
	(if (null? alist) '()
		(let ((item (car alist)))
			(if (not (list? item)) (append (list item) (argsplit_true (cdr alist)))
				(argsplit_true (cdr alist))))))

; Function to split the argument list and extract the false literals, that is with a NOT
(define (argsplit_false alist)
	(if (null? alist) '()
		(let ((item (car alist)))
			(if (list? item) (append (cdr item) (argsplit_false (cdr alist)))
				(argsplit_false (cdr alist))))))	

; Helper function in resolving two lists
(define (compare item blist)
	(cond ((null? blist) (list (append '(NOT) (list item))))
		  ((equal? item (car blist)) (cdr blist))
		  (else (append (list (car blist)) (compare item (cdr blist))))))

; Helper function in resolving two lists 
(define (compare_combine alist blist)
	(if (null? alist) blist
		(let ((clist (compare (car alist) blist)))
			(compare_combine (cdr alist) clist))))

; Function to check if there is a contradiction between the two lists
(define (check_contradiction alist blist)
	(if (and (not (list? (car alist))) (not (list? (car blist)))) #f 
		(if (or (not (equal? (length alist) 1)) (not (equal? (length blist) 1))) #f
			(if (list? (car alist)) 
				(equal? (cdr (car alist)) blist)
				(equal? (cdr (car blist)) alist)))))

; Function to remove duplicate statements from a list of premises
(define (dedupe clist)
	(cond ((null? clist) '())
		  ((member (car clist) (cdr clist)) (dedupe (cdr clist)))
		  (else (cons (car clist) (dedupe (cdr clist))))))
	
; Function to resolve two lists
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
				     	(let ((final (dedupe (append clist2 clist1))))
							(if (null? final) #f
								(if (equal? (length final) (+ (length alist) (length blist))) #f
									(if (member final kb) #f
										final))))))))))

; Helper function to NOT operator
(define (insert_not alist)
	(if (null? alist) '()
		(append (operator_not (list (car alist))) (operator_not (cdr alist)))))		  

; Helper function to take NOT of BICONDITIONAL statement
(define (insert_not_bicond alist)
	(if (null? alist) '()
		(append (list (operator_not (car alist))) (list (operator_not (car (cdr alist)))))))		  

; NOT operator. Returns the CNF form of the input list after taking a NOT
(define (operator_not alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (append '(NOT) (list (car alist)))) (operator_not (cdr alist))))
		  ((equal? (list (car (car alist))) '(NOT)) (append (cdr (car alist)) (operator_not (cdr alist))))
		  ((equal? (list (car (car alist))) '(OR)) (insert_not (operator_or (cdr (car alist)))))
 		  ((equal? (list (car (car alist))) '(IMPLIES)) (insert_not (operator_implies (cdr (car alist)))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (insert_not_bicond (operator_bicond (cdr (car alist)))))))

; OR operator. Returns the CNF form of the input list after taking a OR
(define (operator_or alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (car alist)) (operator_or (cdr alist))))
		  ((equal? (list (car (car alist))) '(NOT)) (append (operator_not (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_not (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(OR)) (append (operator_or (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_or (cdr (cdr (car alist))))))
 		  ((equal? (list (car (car alist))) '(IMPLIES)) (append (operator_implies (cdr (car alist))) (operator_or (cdr alist)))) ;(operator_implies (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (append (operator_bicond (cdr (car alist))) (operator_or (cdr alist)))))) ;(operator_bicond (cdr (car alist))))))

; IMPLIES operator. Returns the CNF form of the input list after applying the IMPLIES operator
(define (operator_implies alist)
	(if (null? alist) '()
		(let ((var1 (operator_not (list (car alist)))))
			(cond ((not (list? (car (cdr alist)))) (append var1 (cdr alist)))
				  ((equal? (list (car (car (cdr alist)))) '(NOT)) (append var1 (operator_not (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(OR)) (append var1 (operator_or (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(IMPLIES)) (append var1 (operator_implies (cdr (car (cdr alist))))))
				  ((equal? (list (car (car (cdr alist)))) '(BICONDITIONAL)) (append var1 (operator_bicond (cdr (car (cdr alist))))))))))

; AND operator. Returns the CNF form of one element in an AND operation.
; Need to call individually for each operand in the AND.
(define (operator_and alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (list (car alist)))
		  ((equal? (list (car (car alist))) '(NOT)) (operator_not (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(OR)) (operator_or (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(IMPLIES)) (operator_implies (cdr (car alist))))
		  ((equal? (list (car (car alist))) '(BICONDITIONAL)) (operator_bicond (cdr (car alist))))))

; BICONDITIONAL operator. Returns the list of two new statements in CNF form after applying BICONDITIONAL elimination
(define (operator_bicond alist)
	(if (null? alist) '()
		(let* ((var1 (operator_implies alist))
			   (var2 (operator_implies (reverse alist))))
			  (append (list var1) (list var2)))))	

; Knowledge base
(define kb '())

; Function to remove duplicate statements from the knowledge base
(define (dedupe_kb clist)
	(cond ((null? clist) '())
		  ((or (member (car clist) (cdr clist)) (member (reverse (car clist)) (cdr clist))) (dedupe_kb (cdr clist)))
		  (else (cons (car clist) (dedupe_kb (cdr clist))))))

; Tell interface. Function to insert statements into the knowledge base
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

; Helper function in resolving the premise in question
(define (resolve_item item plist rlist complist)
	(if (null? plist) rlist
		(let ((resolution (resolve item (car plist))))
		;	 (display '(resolution :)) (display resolution) (newline)
			 (if (equal? resolution #f) (resolve_item item (cdr plist) rlist complist)
				(if (or (member resolution complist) (member (reverse resolution) complist)) (resolve_item item (cdr plist) rlist complist)
				 	(resolve_item item (cdr plist) (append (list resolution) rlist) complist))))))

; Function to resolve the knowledge base after inserting the negation of premise in question into the knowledge base.
;  Called by the ask interface. It accepts 4 parametes :
;  	1. Main list - list 1 of statements involved in resolution
;  	2. Premise list - list 2 of statements involved in resolution
;  	3. Resolution list - list containing the statements generated after resolving list 1 with list 2
;  	4. Complete list - list of all the premises that have been resolved till the time.
; Returns CONTRADICTION if there is a contradiction, indicating that the premise asked is true, else UNKNOWN
(define (resolve_prem mlist plist rlist complist)
	(display '(mlist :)) (display mlist) (newline) (display '(plist :)) (display plist) (newline) (display '(rlist :)) (display rlist) (newline)
	(cond ((null? plist) 'UNKNOWN)
		  ((member '(CONTRADICTION) rlist) '(CONTRADICTION))
		  ((null? mlist) (resolve_prem (dedupe_kb (append complist plist)) rlist '() (dedupe_kb (append complist plist))))
		  (else
			(let ((new_rlist (resolve_item (car mlist) plist rlist (dedupe_kb (append complist plist)))))
				 (resolve_prem (cdr mlist) plist (dedupe_kb new_rlist) complist)))))  

; Ask interface. Function to ask the propositional logic statement. Resolved by inserting the negation of 
; the asked premise into the knowledge base and then resolving to check if there is a CONTRADICTION. If there is,
; it returns #t, else returns UNKNOWN.
(define (ask premise)
	(cond ((null? premise) '())
		  ((not (list? premise)) (let* ((negative (operator_not (list premise)))
			   						   (temp_kb (append (list (list (car negative))) kb))
									   (new_kb (append (list (cdr negative)) temp_kb))
			   						   (result (resolve_prem new_kb new_kb '() new_kb)))
			   						   (if (equal? result '(CONTRADICTION)) #t
				   						   'UNKNOWN)))
		  ((equal? (list (car premise)) '(BICONDITIONAL)) (let* ((negative (operator_not (list premise)))
																 (bicond (operator_bicond (cdr premise))))
															    (if (and (or (member (car bicond) kb) (member (reverse (car bicond)) kb))
																		(or (member (car (cdr bicond)) kb) (member (reverse (car (cdr bicond))) kb))) #t
																 (begin 
															     (let*	((temp_kb1 (append (list (list (car (car negative)))) kb))
																		 (new_kb1 (append (list (cdr (car negative))) temp_kb1))
																		 (temp_kb2 (append (list (list (car (car (cdr negative))))) kb))
																 		 (new_kb2 (append (list (cdr (car (cdr negative)))) temp_kb2))
																 		 (result1 (resolve_prem new_kb1 new_kb1 '() new_kb1)))
																 		(if (equal? result1 '(CONTRADICTION)) (let ((result2 (resolve_prem new_kb2 new_kb2 '() new_kb2)))
																										   			(if (equal? result2 '(CONTRADICTION)) #t 'UNKNOWN))
																	 'UNKNOWN))))))
		  ((equal? (list (car premise)) '(AND)) (let* ((neg1 (operator_not (operator_and (list (car (cdr premise))))))
													   (neg2 (operator_not (operator_and (list (car (cdr (cdr premise)))))))
													   (temp_kb1 (append (list (list (car neg1))) kb))
													   (new_kb1 (append (list (cdr neg1)) temp_kb1))
													   (temp_kb2 (append (list (list (car neg2))) kb))
													   (new_kb2 (append (list (cdr neg2)) temp_kb2))
													   (result1 (resolve_prem new_kb1 new_kb1 '() new_kb1)))
                                                                 (if (equal? result1 '(CONTRADICTION)) (let ((result2 (resolve_prem new_kb2 new_kb2 '() new_kb2)))
                                                                                                           (if (equal? result2 '(CONTRADICTION)) #t 'UNKNOWN))
                                                                     'UNKNOWN)))
		 (else 
			(let* ((negative (operator_not (list premise)))
				   (temp_kb (append (list (list (car negative))) kb))
                   (new_kb (append (list (cdr negative)) temp_kb))
                   (result (resolve_prem new_kb new_kb '() new_kb)))
                   (if (equal? result '(CONTRADICTION)) #t
                       'UNKNOWN)))))
																 
			   





		 
