(define (get-nth index alist)
	(cond ((= 1 index) (car alist))
		  (#t (get-nth (- index 1) (cdr alist)))))

(define (my-eval-fn alist) 
	(let ((a (get-nth 1 alist)))
		(let ((b (get-nth 2 alist)))
			(let ((c (get-nth 3 alist)))
				(let ((d (get-nth 4 alist)))
					(let ((e (get-nth 5 alist)))
						(let ((f (get-nth 6 alist)))
							(+ (* 6 a b) (* 4 c d d)
							   (* -3 a a) (* 46 e f) 
							   (* -5 f c c)))))))))

(define (generate-member size domain)
	(if (zero? size) '()
		(append (list (random domain)) (generate-member (- size 1) domain))))

(define (generate-pop size num domain)
	(if (zero? num) '()
		(append (list (generate-member size domain)) (generate-pop size (- num 1) domain))))	 

(define (evaluate-pop pop eval-fn)
	(if (null? pop) '()
		(if (= (length (car pop)) 6) 
			(let ((fit-val (eval-fn (car pop))))
				(append (list (cons fit-val (car pop))) (evaluate-pop (cdr pop) eval-fn)))
			(let ((fit-val (eval-fn (cdr (car pop)))))
				(append (list (cons fit-val (cdr (car pop)))) (evaluate-pop (cdr pop) eval-fn)))))) 

(define (list-remove alist idx)
	(if (zero? idx) (cdr alist)
		(append (list (car alist)) (list-remove (cdr alist) (- idx 1)))))

(define (shuffle pop)
	(if (null? pop) '()
		(let ((index (random (length pop))))
			(append (list (list-ref pop index)) 
				(shuffle (list-remove pop index))))))

(define (take alist size)
	(if (null? alist) '()
		(if (zero? size) '()
			(append (list (car alist)) (take (cdr alist) (- size 1))))))

(define (compare a b)
	(> (car a) (car b)))

(define (select-two pop size)
	(if (null? pop) '()
		(take (sort (take (shuffle pop) size) compare) 2)))

(define (cross-over mem1 mem2)
	(if (or (null? mem1) (null? mem2)) '()
		(let ((point (random (min (length mem1) (length mem2)))))
			;(display mem1) (newline) (display mem2) (newline) (display (min (length mem1) (length mem2))) (newline) (display point) (newline)
			(append (list (append (take mem1 (+ point 1)) (list-tail mem2 (+ point 1))))
					(list (append (take mem2 (+ point 1)) (list-tail mem1 (+ point 1))))))))	

(define (mate-pop eval-pop pop-size select-size)
	(if (null? eval-pop) '()
		(if (zero? pop-size) '()
			(let ((members-to-mate (select-two eval-pop select-size)))
		;		(display members-to-mate) (newline)
				(let ((children (cross-over (car members-to-mate) (car (cdr members-to-mate)))))
			;		(display children) (newline)
					(append children (mate-pop eval-pop (- pop-size 2) select-size))))))) 
	
(define (mutate-mem mem domain mutation-rate)
	(if (null? mem) '()
		(if (< (random domain) mutation-rate) 
			(append (list (random domain)) (mutate-mem (cdr mem) domain mutation-rate))
			(append (list (car mem)) (mutate-mem (cdr mem) domain mutation-rate)))))

(define (mutate-pop eval-pop domain mutation-rate)
	(if (null? eval-pop) '()
		(append (list (cons (car (car eval-pop)) (mutate-mem (cdr (car eval-pop)) domain mutation-rate))) (mutate-pop (cdr eval-pop) domain mutation-rate))))

(define (get-best-member pop)
	(if (null? pop) '()
		(car (sort pop compare))))

(define (set-global-best cur-best best-mem)
	(if (null? best-mem) cur-best
		(if (> (car cur-best) (car best-mem)) cur-best best-mem))) 

(define (calc-fitness-sum pop)
	(if (null? pop) 0
		(+ (car (car pop)) (calc-fitness-sum (cdr pop)))))

(define (calc-fitness-avg pop size)
	(if (null? pop) 0
		(/ (/ (calc-fitness-sum pop) size) 1.0)))

(define (create-new-gen gens init-pop pop-size domain select-size mutation-rate best-mem my-eval-func)
	(if (zero? gens) (get-best-member init-pop) ;(begin (display '(best mem :-)) (display best-mem) (newline)); (get-best-member init-pop)
		;(let ((pop (generate-pop 6 8 20)))
			(let ((eval-pop (evaluate-pop init-pop my-eval-func)))
			;	(display eval-pop) (newline)
				(let ((avg-fit (calc-fitness-avg eval-pop pop-size)))
				;	(display '(avg fitness :)) (display gens) (display '(:)) (display avg-fit) (newline)
					(let ((cur-best (get-best-member eval-pop)))
				;		(display cur-best) (newline)
						(let ((global-best (set-global-best cur-best best-mem)))
							(let ((offsprings (mate-pop eval-pop pop-size select-size)))
								;(display offsprings) (newline)
								(let ((mutated-pop (mutate-pop offsprings domain mutation-rate)))
				;					(display mutated-pop) (newline)
									(create-new-gen (- gens 1) mutated-pop pop-size domain select-size mutation-rate global-best my-eval-func)))))))));)

(define (search-ga my-eval-func)
	(let ((pop (generate-pop 6 50 20)))
		(let ((gens (/ 50000 50)))
			(create-new-gen gens pop 50 20 25 10 '() my-eval-func)))) 

