(define all-moves '(((1 2 4) (1 3 6)) ((2 4 7) (2 5 9)) ((3 5 8) (3 6 10))
		    ((4 7 11) (4 8 13) (4 5 6) (4 2 1)) ((5 8 12) (5 9 14))
		    ((6 9 13) (6 10 15) (6 5 4) (6 3 1)) ((7 4 2) (7 8 9))
		    ((8 9 10) (8 5 3)) ((9 8 7) (9 5 2)) ((10 6 3) (10 9 8))
		    ((11 7 4) (11 12 13)) ((12 13 14) (12 8 5)) 
		    ((13 14 15) (13 12 11) (13 9 6) (13 8 4)) ((14 13 12) (14 9 5))
		    ((15 14 13) (15 10 6))))

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
	;(display state-list) (newline)
	(if (or (null? state-list) (null? goal-state)) '();(begin (display #f) (display final-states) (newline))
		(let ((cur-state (car state-list)))
			(if (null? cur-state) (dfs (cdr state-list) goal-state)
				 (if (equal? cur-state goal-state) (append (list cur-state) (list #t));(begin (display #t) (newline) (display cur-state) (newline) (display final-states) (newline))
					(let ((child-states (get-children cur-state)))
						(if (null? child-states) (dfs (cdr state-list) goal-state)
							(begin
							;(display '(cur-state -)) (display cur-state) (display '(child-states -)) (display child-states) (newline)
							(let (( result (dfs (append-child-states child-states (cdr state-list)) goal-state)))
								;(display result) (newline)
								(if (null? result) result
									(if (null? (car result)) result
										(if (member (car result) child-states) (append (list cur-state) result)
											result))))
							;(display '(final states -)) (display final-states) (newline)
							;(display cur-state) (newline)
							))))))))

(define (get-new-peg cur-state next-state)
	(if (member (car next-state) cur-state) (get-new-peg cur-state (cdr next-state))
		(car next-state)))

(define (get-move cur-state poss-moves next-state)
	(if (and (member (car (reverse (car poss-moves))) cur-state) (not (member (car (reverse (car poss-moves))) next-state))) (reverse (car poss-moves))
		(get-move cur-state (cdr poss-moves) next-state)))

(define (extract-moves final-states next-state)
	(if (null? final-states) '()
		(if (null? (cdr next-state)) (list (append (car final-states) next-state))
			;(if (null? (cdr (cdr next-state))) (list next-state)
				(let ((new-peg (get-new-peg (car final-states) next-state)))
					(let ((poss-moves (list-ref all-moves (- new-peg 1))))
						(let ((move (get-move (car final-states) poss-moves next-state)))
						;	(display move) (newline)
							(append (list move) (extract-moves (cdr final-states) (car (cdr (cdr final-states)))))))))));)		

(define (peg-basic state-list goal-state)
	(if (or (null? state-list) (null? goal-state)) (begin (display #f) (display '(Incorrect Input!)) (newline))
		(let ((final-states (dfs (list state-list) goal-state)))
			(if (null? final-states) #f
				(if (equal? (car final-states) goal-state) #t
					(begin (display '(state of board: )) (display final-states) (newline)
						(let ((moves (extract-moves final-states (car (cdr final-states)))))
							(display '(moves made: )) (display moves) (newline))))))))
	
