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
	(if (not (equal? (length alist) 1)) #f
		(if (list? (car alist)) 
			(equal? (cdr (car alist)) blist)
			(equal? (cdr (car blist)) alist))))

(define (dedupe clist)
	(cond ((null? clist) '())
		  ((member (car clist) (cdr clist)) (dedupe (cdr clist)))
		  (else (cons (car clist) (dedupe (cdr clist))))))
	

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
				  ;   (display clist1) (newline) (display clist2) (newline)
				     (let ((final (dedupe (append clist1 clist2))))
						(cond ((null? final) #f)
							  ((equal? (length final) (+ (length alist) (length blist))) #f)
							  ((> (- (+ (length alist) (length blist)) (length final)) 2) #f)
							  (else	final))))))))
		  
		  
