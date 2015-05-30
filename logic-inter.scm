;; Assignment 6 - Automated Inference - Theorem Prover : Intermediate
;
; The program implements the Propositional Logic Solver that accepts statements in CNF form and 
; performs resolution to answer propositional logic questions. The program implements a 
; "tell" interface to generate a knowledge base and an "ask" interface to ask the logic questions.
; The algorithm adds the negative of the asked premise into the knowledge base and checks if on resolving
; the clauses, a contradiction occurs. 
; If there is a contradiction, the asked premise is true and the program returns #t. 
; If there is no contradiction, then the program returns UNKNOWN.
;
; The program accepts strings only in the CNF form (in the same format as in the description).
; The resolution logic is same as the basic assignment.
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

; Resolves two lists in CNF form to generate either the resolved clause or #f or a CONTRADICTION.
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
							(cond ((null? final) #f)
                              	  ((equal? (length final) (+ (length alist) (length blist))) #f)
                              	  ((member final kb) #f)
							  	  (else final)))))))))

; NOT operator. To take the negation of the asked premise	
(define (operator_not alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (append '(NOT) (list (car alist)))) (operator_not (cdr alist))))
		  (else (append (cdr (car alist)) (operator_not (cdr alist))))))

; Knowledge base maintained in list 'kb'
(define kb '())

; Function to dedupe the knowledge base - remove any repeated statements
(define (dedupe_kb clist)
    (cond ((null? clist) '())
          ((or (member (car clist) (cdr clist)) (member (reverse (car clist)) (cdr clist))) (dedupe_kb (cdr clist)))
          (else (cons (car clist) (dedupe_kb (cdr clist))))))

; Tell interface to generate the knowledge base. The statement entered is in CNF form.
(define (tell premise)
	(if (null? premise) '(EMPTY PREMISE)
		(set! kb (append (list premise) kb)))
	(set! kb (dedupe_kb kb))
    (display 'OK) (newline))

; Called by the resolve_prem(). Resolves one statement from main list with the whole premise list.
(define (resolve_item item plist rlist complist)
    (if (null? plist) rlist
        (let ((resolution (resolve item (car plist))))
             (if (equal? resolution #f) (resolve_item item (cdr plist) rlist complist)
                (if (or (member resolution complist) (member (reverse resolution) complist)) (resolve_item item (cdr plist) rlist complist)
                    (resolve_item item (cdr plist) (append (list resolution) rlist) complist))))))

; Called by the ask interface. It accepts 4 parametes :
; 	1. Main list - list 1 of statements involved in resolution
; 	2. Premise list - list 2 of statements involved in resolution
; 	3. Resolution list - list containing the statements generated after resolving list 1 with list 2
; 	4. Complete list - list of all the premises that have been resolved till the time.
;
; In each recurssion, the function checks if a CONTRADICTION has been encountered. If yes, return else continue resolving.
(define (resolve_prem mlist plist rlist complist)
    (cond ((null? plist) 'UNKNOWN)
          ((member '(CONTRADICTION) rlist) '(CONTRADICTION))
          ((null? mlist) (resolve_prem (dedupe_kb (append complist plist)) rlist '() (dedupe_kb (append complist plist))))
          (else
            (let ((new_rlist (resolve_item (car mlist) plist rlist (dedupe_kb (append complist plist)))))
                 (resolve_prem (cdr mlist) plist (dedupe_kb new_rlist) complist)))))

; Ask interface to check if the given premise is true or UNKNOWN. It takes the NOT of the premise
; and adds it to the knowldge base, which is then given for resolution.
(define (ask premise)
	(if (null? premise) '()
		(let* ((negative (operator_not premise))
                   (temp_kb (append (list (list (car negative))) kb))
				   (new_kb (append (list (cdr negative)) temp_kb))
                   (result (resolve_prem new_kb new_kb '() new_kb)))
                   (if (equal? result '(CONTRADICTION)) #t
                       'UNKNOWN))))		  

 
