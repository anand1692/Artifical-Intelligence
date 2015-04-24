(define all-moves '(((1 2 4) (1 3 6)) ((2 4 7) (2 5 9)) ((3 5 8) (3 6 10))
		    ((4 8 12) (4 5 6) (4 2 1)) ((5 8 11) (5 9 13))
		    ((6 9 12) (6 5 4) (6 3 1)) ((7 4 2) (7 8 9) (7 11 14))
		    ((8 9 10) (8 5 3) (8 12 15)) ((9 8 7) (9 5 2) (9 12 14)) ((10 6 3) (10 9 8) (10 13 15))
		    ((11 8 5) (11 12 13) (11 14 16)) ((12 9 6) (12 8 4)) 
		    ((13 15 16) (13 12 11) (13 9 5)) ((14 12 9) (14 11 7))
		    ((15 13 10) (15 12 8)) ((16 15 13) (16 14 11))))

(define (validate-moves open-list moves-list)
	(if (null? moves-list) '()
		(let ((temp-move (car moves-list)))
			;(display temp-move) (newline) (display (car (cdr temp-move))) (newline) (display (car (cdr (cdr temp-move)))) (newline)
			(if (and (member (car (cdr temp-move)) open-list) 
			    	 (not (member (car (cdr (cdr temp-move))) open-list)))
				; (cons #t (validate-moves open-list (cdr moves-list)))
				; (cons #f (validate-moves open-list (cdr moves-list)))))))
				(cons temp-move (validate-moves open-list (cdr moves-list)))
			   	(validate-moves open-list (cdr moves-list))))))  

(define (gen-moves state-list elem)
	(if (or (null? state-list) (null? (cdr state-list))) '()
		(let ((poss-moves (list-ref all-moves (- elem 1))))
			;(display poss-moves) (newline)	
 			(let ((legal-moves (validate-moves state-list poss-moves)))
			;	(display legal-moves) (newline)
				(if (null? (cdr (memq elem state-list))) legal-moves 
					(if (null? legal-moves) (gen-moves state-list (car (cdr (memq elem state-list))))
						(append legal-moves (gen-moves state-list (car (cdr (memq elem state-list)))))))))))

(define (delete state-list elem)	
	(if (equal? elem (car state-list)) (cdr state-list)
		(cons (car state-list) (delete (cdr state-list) elem))))
	
(define (add state-list elem)
	(if (null? state-list) (cons elem state-list)
	(if (> (car state-list) elem) (cons elem state-list)
		(cons (car state-list) (add (cdr state-list) elem)))))

(define (apply-moves state-list legal-moves)
	(if (null? legal-moves) state-list
		(let ((move (car legal-moves)))
			(let ((temp-state1 (delete state-list (car move)))) 
				(let ((temp-state2 (delete temp-state1 (car (cdr move)))))
					(let ((new-state (add temp-state2 (car (cdr (cdr move))))))
						(if (null? (cdr legal-moves)) (list new-state)
							(cons new-state (apply-moves state-list (cdr legal-moves))))))))))

(define (get-children state-list)
	(if (null? state-list) '()
		(let ((legal-moves (gen-moves state-list (car state-list)))) 
			(if (null? legal-moves) '()
				(apply-moves state-list legal-moves)))))

(define (append-child-states child-states state-list)
	(if (null? child-states) state-list
		(cons (car child-states) (append-child-states (cdr child-states) state-list))))

(define (dfs state-list goal-state) 
;	(display state-list) (newline)
	(if (or (null? state-list) (null? goal-state)) #f
		(let ((cur-state (car state-list)))
			(if (null? cur-state) (dfs (cdr state-list) goal-state)
				;((display cur-state) (newline) 
				 (if (equal? cur-state goal-state) (begin (display '(#t)) (newline) (display cur-state) (newline))
					(let ((child-states (get-children cur-state)))
						(if (null? child-states) (dfs (cdr state-list) goal-state)
							(begin
							(dfs (append-child-states child-states (cdr state-list)) goal-state)
							;(display cur-state) (newline)
							))))))))

	