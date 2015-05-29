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
                       	(let ((final (dedupe (append clist2 clist1))))
                            (if (null? final) #f
                            	(if (equal? (length final) (+ (length alist) (length blist))) #f
                               		(if (member final kb) #f
                                  		final))))))))))
	
(define (operator_not alist)
	(cond ((null? alist) '())
		  ((not (list? (car alist))) (append (list (append '(NOT) (list (car alist)))) (operator_not (cdr alist))))
		  (else (append (cdr (car alist)) (operator_not (cdr alist))))))

(define kb '())

(define (dedupe_kb clist)
    (cond ((null? clist) '())
          ((or (member (car clist) (cdr clist)) (member (reverse (car clist)) (cdr clist))) (dedupe_kb (cdr clist)))
          (else (cons (car clist) (dedupe_kb (cdr clist))))))

(define (tell premise)
	(if (null? premise) '(EMPTY PREMISE)
		(set! kb (append (list premise) kb)))
	(set! kb (dedupe_kb kb))
    (display 'OK) (newline))

(define (resolve_item item plist rlist complist)
    (if (null? plist) rlist
        (let ((resolution (resolve item (car plist))))
             (if (equal? resolution #f) (resolve_item item (cdr plist) rlist complist)
                (if (or (member resolution complist) (member (reverse resolution) complist)) (resolve_item item (cdr plist) rlist complist)
                    (resolve_item item (cdr plist) (append (list resolution) rlist) complist))))))

(define (resolve_prem mlist plist rlist complist)
;	(display '(mlist :)) (display mlist) (newline) (display '(plist :)) (display plist) (newline) (display '(rlist :)) (display rlist) (newline)
    (cond ((null? plist) '(UNKNOWN))
          ((member '(CONTRADICTION) rlist) '(CONTRADICTION))
          ((null? mlist) (resolve_prem (dedupe_kb (append complist plist)) rlist '() (dedupe_kb (append complist plist))))
          (else
            (let ((new_rlist (resolve_item (car mlist) plist rlist (dedupe_kb (append complist plist)))))
                 (resolve_prem (cdr mlist) plist (dedupe_kb new_rlist) complist)))))
	
(define (ask premise)
	(if (null? premise) '()
		(let* ((negative (operator_not premise))
                   (new_kb (append (list negative) kb))
                   (result (resolve_prem new_kb new_kb '() new_kb)))
                   (if (equal? result '(CONTRADICTION)) #t
                       'UNKNOWN))))		  

 
