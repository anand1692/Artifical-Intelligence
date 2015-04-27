; Assignment 3 - Intermediate Implementation of Cracker Barrel's Peg game in Scheme
;
; The algorithm uses depth-first-search algorithm to determine if a given goal state can be reached 
; from a given initial state. Goal state and Initial state are the two input parameters.
; The program uses backtracking concept to determine the path from the initial state to the goal state. 
; At the end, if the goal-state is reachable,  all the required moves in forward order 
; and the state of the board after each move is displayed back to the user. 
;
; In this implementation, there are total 16 pegs arranged in a form of a diamond and each position
; is represented by a number. It looks like:
;                               1
;                            2     3
;                          4    5    6
;                        7    8    9   10
;                         11    12   13
;                            14    15
;                               16
;
; Created by Anand Goyal © 2015, April 2015 


; This definition holds all the possible moves from a particular position on the board.
; The peg can only move if it can jump over an immediate adjacent peg. Hence at the end of
; each legal move, there will be one peg less on the board. 
;
(define all-moves '(((1 2 4) (1 3 6)) ((2 4 7) (2 5 9)) ((3 5 8) (3 6 10))
		    ((4 8 12) (4 5 6) (4 2 1)) ((5 8 11) (5 9 13))
		    ((6 9 12) (6 5 4) (6 3 1)) ((7 4 2) (7 8 9) (7 11 14))
		    ((8 9 10) (8 5 3) (8 12 15)) ((9 8 7) (9 5 2) (9 12 14)) ((10 6 3) (10 9 8) (10 13 15))
		    ((11 8 5) (11 12 13) (11 14 16)) ((12 9 6) (12 8 4)) 
		    ((13 15 16) (13 12 11) (13 9 5)) ((14 12 9) (14 11 7))
		    ((15 13 10) (15 12 8)) ((16 15 13) (16 14 11))))

; This function takes the current state of the board as open list and a list of all possible moves 
; from a particular peg on the board, as input. It then validates and returns a list of 
; only the legal moves from that peg according to the current state of the board.
;
(define (validate-moves open-list moves-list)
	(if (null? moves-list) '()
		(let ((temp-move (car moves-list)))
			(if (and (member (car (cdr temp-move)) open-list) 
			    	 (not (member (car (cdr (cdr temp-move))) open-list)))
				(cons temp-move (validate-moves open-list (cdr moves-list)))
			   	(validate-moves open-list (cdr moves-list))))))  

; This function generates all possible legal moves on the current state of board. 
; It takes in the current state of the board and the peg whose legal moves have to determined as input.
; It iterates through all the pegs in the current state and returns the list of all possible
; legal moves in the current state of the board. 
;
(define (gen-moves state-list elem)
	(if (or (null? state-list) (null? (cdr state-list))) '()
		(let ((poss-moves (list-ref all-moves (- elem 1))))
 			(let ((legal-moves (validate-moves state-list poss-moves)))
				(if (null? (cdr (memq elem state-list))) legal-moves 
					(if (null? legal-moves) (gen-moves state-list (car (cdr (memq elem state-list))))
						(append legal-moves (gen-moves state-list (car (cdr (memq elem state-list)))))))))))

; This function removes a particular peg from the particular state of the current board.
;
(define (delete state-list elem)	
	(if (equal? elem (car state-list)) (cdr state-list)
		(cons (car state-list) (delete (cdr state-list) elem))))

; This function adds a particular peg to a particular state on the current board and inserts into the list
; in ascending order.   
;	
(define (add state-list elem)
	(if (null? state-list) (cons elem state-list)
	(if (> (car state-list) elem) (cons elem state-list)
		(cons (car state-list) (add (cdr state-list) elem)))))

; This function applies the list of legal moves on the current state of board and generates a new state for 
; each of the move made. It returns a list of new states of the board created after making each legal move 
; individually and independently to each other.
;
(define (apply-moves state-list legal-moves)
	(if (null? legal-moves) state-list
		(let ((move (car legal-moves)))
			(let ((temp-state1 (delete state-list (car move)))) 
				(let ((temp-state2 (delete temp-state1 (car (cdr move)))))
					(let ((new-state (add temp-state2 (car (cdr (cdr move))))))
						(if (null? (cdr legal-moves)) (list new-state)
							(cons new-state (apply-moves state-list (cdr legal-moves))))))))))

; This function calls the (gen-moves) and (apply-moves) and returns the list of all possible legal states
; that can be reached from the current state of the board. It calls the new states as children of the current
; state. It takes the current state list as input.
;
(define (get-children state-list)
	(if (null? state-list) '()
		(let ((legal-moves (gen-moves state-list (car state-list)))) 
			(if (null? legal-moves) '()
				(apply-moves state-list legal-moves)))))

; This function appends the children of the current-state to the front of the list after popping the current state.
; The children are appended to the front of the list as the prime feature of DEPTH-FIRST search.
;
(define (append-child-states child-states state-list)
	(if (null? child-states) state-list
		(cons (car child-states) (append-child-states (cdr child-states) state-list))))

; This is the main DEPTH-FIRST search function which maintains the open list from the algorithm, called the state-list, which contains 
; the list of all the states of the board after making all possible legal moves from each state. The algorithm explores
; depth first, i.e., one particular parent node completely before on to the next node at the same level. 
; The algorithm is as follows:
;
; 1. Pop the state at the front of the list. Let this be the current state.
; 2. Check if the current state is the goal state. If yes, return.
; 3. If no, generate all the possible legal child states of the current state and add them to the front of the list.
; 4. Recurse by again popping the front of the list.
;
; Once the goal state is reached, the algorithm backtracks and generates a list of all the intermediate states.
; If the goal state is reachable, this list of intermediate states is returned, else a null list is returned. 
;
(define (dfs state-list goal-state) 
	(if (or (null? state-list) (null? goal-state)) '()
		(let ((cur-state (car state-list)))
			(if (null? cur-state) (dfs (cdr state-list) goal-state)
				 (if (equal? cur-state goal-state) (append (list cur-state) (list #t))
					(let ((child-states (get-children cur-state)))
						(if (null? child-states) (dfs (cdr state-list) goal-state)
							(begin
							(let (( result (dfs (append-child-states child-states (cdr state-list)) goal-state)))
								(if (null? result) result
									(if (null? (car result)) result
										(if (member (car result) child-states) (append (list cur-state) result)
											result))))))))))))

; This function returns the new peg in the current state as compared to the previous state.
; It is used as one of the functions while determining the moves made to reach the goal state.
;
(define (get-new-peg cur-state next-state)
	(if (member (car next-state) cur-state) (get-new-peg cur-state (cdr next-state))
		(car next-state)))

; This function returns the move made from the previous state to reach the current state.
; It is used as one of the functions while determing the moves from to reach the goal state.
;
(define (get-move cur-state poss-moves next-state)
	(if (and (member (car (reverse (car poss-moves))) cur-state) (not (member (car (reverse (car poss-moves))) next-state))) (reverse (car poss-moves))
		(get-move cur-state (cdr poss-moves) next-state)))

; This function returns the list of moves made from the initial state to reach the goal state. It takes the list of
; all the states, including the intermediate states, to the reach the goal state as input. It then calls the (get-new-peg) and 
; (get-move) to get the move at each transition and finally returns the list of moves made to reach the goal state.
;
(define (extract-moves final-states next-state goal-state)
	(if (null? final-states) '()
		(if (equal? (car final-states) goal-state) (list (car final-states))
			(if (null? (cdr next-state)) (list (append (car final-states) next-state))
				(let ((new-peg (get-new-peg (car final-states) next-state)))
					(let ((poss-moves (list-ref all-moves (- new-peg 1))))
						(let ((move (get-move (car final-states) poss-moves next-state)))
							(append (list move) (extract-moves (cdr final-states) (car (cdr (cdr final-states))) goal-state)))))))))		

; This the main calling function which should be called by the user to determine if the goal state can be reached
; from an initial state. It takes the intial state and goal state as the input parameters and returns true or false if the goal
; state is reached or not correspondingly. If the return value is true, it also returns the list of moves made to reach the goal
; state and the state of the board after each move is made.
;	
(define (peg-intermediate state-list goal-state)
	(if (or (null? state-list) (null? goal-state)) (begin (display #f) (display '(Incorrect Input!)) (newline))
		(let ((final-states (dfs (list state-list) goal-state)))
			(if (null? final-states) #f
				(if (equal? (car final-states) goal-state) #t
					(begin (display '(state of board: )) (display final-states) (newline)
						(let ((moves (extract-moves final-states (car (cdr final-states)) goal-state)))
							(display '(moves made: )) (display moves) (newline))))))))
