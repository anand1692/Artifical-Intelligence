;; Assignment 6 - Automated Inference - Theorem Prover : Basic 
;
; The assignment implements the resolution algorithm. It accepts two statements in their CNF form
; and returns their resolution, if it exists. The resolution algorithm works as follows :
;	1. Literal resolved with its own compliment gives a contradiction : (a) ^ (NOT a) = CONTRADICTION
;	2. If there are no pairs of complimentary literals between the two statements : returns #f -> (a b) ^ (c d)
;	3. If there are more than one pair of complimentary literals between the two statements : returns #f -> (a b) ((NOT a) (NOT b))
;	4. If there is exactly one pair of complimentary literals between the two statements : returns resolved clause -> (a b) ^ ((NOT a) c) = (b c)
;
; The function "resolve" accepts two lists (in same format as given in description) and returns the result. 
;
; Created by Anand Goyal © May 2015.

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
	(if (not (equal? (length alist) 1)) #f
		(if (list? (car alist)) 
			(equal? (cdr (car alist)) blist)
			(equal? (cdr (car blist)) alist))))

; Function to remove duplicate statements from a list of premises
(define (dedupe clist)
	(cond ((null? clist) '())
		  ((member (car clist) (cdr clist)) (dedupe (cdr clist)))
		  (else (cons (car clist) (dedupe (cdr clist))))))
	
; Function to resolve two lists
(define (resolve alist blist)
	(if (null? alist) blist
		(if (null? blist) alist
			(if (check_contradiction alist blist) 'CONTRADICTION 
			  (let* ((tlist1 (argsplit_true alist))
					 (flist1 (argsplit_false alist))
				     (tlist2 (argsplit_true blist))
				     (flist2 (argsplit_false blist))
				     (clist1 (compare_combine flist1 tlist2))
				     (clist2 (compare_combine flist2 tlist1)))
				     (let ((final (dedupe (append clist1 clist2))))
						(cond ((null? final) #f)
							  ((equal? (length final) (+ (length alist) (length blist))) #f)
							  ((> (- (+ (length alist) (length blist)) (length final)) 2) #f)
							  (else	final))))))))
		  
		  
