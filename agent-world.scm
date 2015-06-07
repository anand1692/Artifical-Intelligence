; Assignment 7 - Natural Selection
;
; The program designs an agent that will operate in a virtual world, populated with vegetations, predators 
; and other agents. Agents can consume vegetation (when it is blooming) to increase their energy. An agent's energy
; will also decrease over time based on its actions. Agent will receive percepts of its surroundings. The agent
; will be able to see at 45 degrees in front of it, in either direction upto 5 squares deep. Agent will also receive
; a list of previous events, which will indicate what event occired in the previous turn. 
;
; Agent's aim is to survive in the environment while the simulations runs and gain maximum energy. The agent runs
; in different environemnts based on the probability of vegetation, predators and size of the land. In each call to
; the agent, the "choose-action" function will be called, whose return value will be the action chosen by the agent 
; based on the percepts it receives. 
;
; The following are the agent's actuators :
; 	1. STAY
; 	2. TURN-{LEFT, RIGHT, AROUND}
; 	3. EAT-{PASSIVE, AGGRESSIVE}
; 	4. MOVE-{PASSIVE, AGGRESSIVE}-{1,2,3}
;
; The follwing are the previous events that the agent may receive as a list
; 	1. (moved spaces) : moved "spaces" in the direction it is facing
; 	2. (ate id e-delta) : ate "e-delta" amount of energy from the vegetation in front
; 	3. (fought id e-delta) : lost "e-delta" energy on fighting with another agent over space or vegetation
; 	4. (attacked-by id e-delta) : lost "e-delta" energy on being attacked by a predator
;
; The environment percepts can be the following :
; 	1. empty
; 	2. barrier
; 	3. (vegetation id bloom)
; 	4. (predator id)
; 	5. (agent id strength direction)
;
; The agent with the maximum energy at the end of the simulation is the winner
;
; Created by Anand Goyal © June 2015 

; The vegetation map persistant along an environemnt, to keep track of vegetation 
(define veg-map '())

; Current position of the agent, relative to point it started, initally set to (0 0)
(define my-pos '(0 0))

;Current direction of the agent, intially set to 'east
(define my-dir '(east))

;Get positions of each element of the first percept - length 3 
(define (get-list1 cur_pos)
	(cond ((equal? my-dir '(east))  (let* ((a (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 1))))
		   							(b (append (list (+ (car cur_pos) 1)) (cdr cur_pos)))
		   							(c (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 1)))))
		   							(append (append (list a) (list b)) (list c))))
	 	  ((equal? my-dir '(west))  (let* ((a (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 1))))
		   							(b (append (list (- (car cur_pos) 1)) (cdr cur_pos)))
		   							(c (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 1)))))
		   							(append (append (list a) (list b)) (list c))))
	 	  ((equal? my-dir '(north)) (let* ((a (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 1))))
		   							(b (append (list (car cur_pos)) (list (+ (car (cdr cur_pos)) 1))))
		   							(c (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 1)))))
		   							(append (append (list a) (list b)) (list c))))
	 	  ((equal? my-dir '(south)) (let* ((a (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 1))))
		   							(b (append (list (car cur_pos)) (list (- (car (cdr cur_pos)) 1))))
		   							(c (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 1)))))
		   							(append (append (list a) (list b)) (list c))))))

; Get positions of each element of the second percept - length 5
(define (get-list2 cur_pos)
	(cond ((equal? my-dir '(east)) (let* ((a (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 2))))
		   							(b (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 1))))
		   							(c (append (list (+ (car cur_pos) 2)) (cdr cur_pos)))
		   							(d (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 1))))
		   							(e (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 2)))))
		   							(append (append (append (append (list a) (list b)) (list c)) (list d)) (list e))))
		  ((equal? my-dir '(west)) (let* ((a (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 2))))
		   							(b (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 1))))
		   							(c (append (list (- (car cur_pos) 2)) (cdr cur_pos)))
		   							(d (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 1))))
		   							(e (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 2)))))
		   							(append (append (append (append (list a) (list b)) (list c)) (list d)) (list e))))
		  ((equal? my-dir '(north)) (let* ((a (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 2))))
		   							(b (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 2))))
		   							(c (append (list (car cur_pos)) (list (+ (car (cdr cur_pos)) 2))))
		   							(d (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 2))))
		   							(e (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 2)))))
		   							(append (append (append (append (list a) (list b)) (list c)) (list d)) (list e))))
		  ((equal? my-dir '(south)) (let* ((a (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 2))))
		   							(b (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 2))))
		   							(c (append (list (car cur_pos)) (list (- (car (cdr cur_pos)) 2))))
		   							(d (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 2))))
		   							(e (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 2)))))
		   							(append (append (append (append (list a) (list b)) (list c)) (list d)) (list e))))))

; Get positions of each element of the third percept - length 7
(define (get-list3 cur_pos)
	(cond ((equal? my-dir '(east)) (let* ((a (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 3))))
		   							(b (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 2))))
		   							(c (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 1))))
		   							(d (append (list (+ (car cur_pos) 3)) (cdr cur_pos)))
		   							(e (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 1))))
		   							(f (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 2))))
		   							(g (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 3)))))
		   							(append (append (append (append (append (append (list a) (list b)) (list c)) (list d)) 
								   							(list e)) (list f)) (list g))))
		  ((equal? my-dir '(west)) (let* ((a (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 3))))
		   							(b (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 2))))
		   							(c (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 1))))
		   							(d (append (list (- (car cur_pos) 3)) (cdr cur_pos)))
		   							(e (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 1))))
		   							(f (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 2))))
		   							(g (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 3)))))
		   							(append (append (append (append (append (append (list a) (list b)) (list c)) (list d)) 
								   							(list e)) (list f)) (list g))))
		  ((equal? my-dir '(north)) (let* ((a (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 3))))
		   							(b (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 3))))
		   							(c (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 3))))
		   							(d (append (list (car cur_pos)) (list (+ (car (cdr cur_pos)) 3))))
		   							(e (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 3))))
		   							(f (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 3))))
		   							(g (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 3)))))
		   							(append (append (append (append (append (append (list a) (list b)) (list c)) (list d)) 
								   							(list e)) (list f)) (list g))))
		  ((equal? my-dir '(south)) (let* ((a (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 3))))
		   							(b (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 3))))
		   							(c (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 3))))
		   							(d (append (list (car cur_pos)) (list (- (car (cdr cur_pos)) 3))))
		   							(e (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 3))))
		   							(f (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 3))))
		   							(g (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 3)))))
		   							(append (append (append (append (append (append (list a) (list b)) (list c)) (list d)) 
								   							(list e)) (list f)) (list g))))))

; Get positions of each element of the fourth percept - length 9
(define (get-list4 cur_pos)
	(cond ((equal? my-dir '(east)) (let* ((a (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 4))))
		   							(b (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 3))))
		   							(c (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 2))))
		   							(d (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 1))))
		  							(e (append (list (+ (car cur_pos) 4)) (cdr cur_pos)))
		   							(f (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 1))))
		   							(g (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 2))))
		   							(h (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 3))))
		   							(i (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 4)))))
		   							(append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   							(list d)) (list e)) (list f)) (list g))
														   							(list h)) (list i))))
		  ((equal? my-dir '(west)) (let* ((a (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 4))))
		   							(b (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 3))))
		   							(c (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 2))))
		   							(d (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 1))))
		  							(e (append (list (- (car cur_pos) 4)) (cdr cur_pos)))
		   							(f (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 1))))
		   							(g (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 2))))
		   							(h (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 3))))
		   							(i (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 4)))))
		   							(append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   							(list d)) (list e)) (list f)) (list g))
														   							(list h)) (list i))))
		  ((equal? my-dir '(north)) (let* ((a (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 4))))
		   							(b (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 4))))
		   							(c (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 4))))
		   							(d (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 4))))
		  							(e (append (list (car cur_pos)) (list (+ (car (cdr cur_pos)) 4))))
		   							(f (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 4))))
		   							(g (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 4))))
		   							(h (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 4))))
		   							(i (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 4)))))
		   							(append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   							(list d)) (list e)) (list f)) (list g))
														   							(list h)) (list i))))
		  ((equal? my-dir '(south)) (let* ((a (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 4))))
		   							(b (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 4))))
		   							(c (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 4))))
		   							(d (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 4))))
		  							(e (append (list (car cur_pos)) (list (- (car (cdr cur_pos)) 4))))
		   							(f (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 4))))
		   							(g (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 4))))
		   							(h (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 4))))
		   							(i (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 4)))))
		   							(append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   							(list d)) (list e)) (list f)) (list g))
														   							(list h)) (list i))))))

; Get positions of each element of the fifth percept - length 11
(define (get-list5 cur_pos)
	(cond ((equal? my-dir '(east)) (let* ((a (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 5))))
		   							(b (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 4))))
		   							(c (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 3))))
		   							(d (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 2))))
		   							(e (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 1))))
		   							(f (append (list (+ (car cur_pos) 5)) (cdr cur_pos)))
		   							(g (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 1))))
		  				 			(h (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 2))))
		   							(i (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 3))))
		   							(j (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 4))))
		   							(k (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 5)))))
		   							(append (append (append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   											(list d)) (list e)) (list f)) (list g))
														   											(list h)) (list i)) (list j)) (list k))))
		  ((equal? my-dir '(west)) (let* ((a (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 5))))
		   							(b (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 4))))
		   							(c (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 3))))
		   							(d (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 2))))
		   							(e (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 1))))
		   							(f (append (list (- (car cur_pos) 5)) (cdr cur_pos)))
		   							(g (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 1))))
		  				 			(h (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 2))))
		   							(i (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 3))))
		   							(j (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 4))))
		   							(k (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 5)))))
		   							(append (append (append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   											(list d)) (list e)) (list f)) (list g))
														   											(list h)) (list i)) (list j)) (list k))))
		  ((equal? my-dir '(north)) (let* ((a (append (list (- (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 5))))
		   							(b (append (list (- (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 5))))
		   							(c (append (list (- (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 5))))
		   							(d (append (list (- (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 5))))
		   							(e (append (list (- (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 5))))
		   							(f (append (list (car cur_pos)) (list (+ (car (cdr cur_pos)) 5))))
		   							(g (append (list (+ (car cur_pos) 1)) (list (+ (car (cdr cur_pos)) 5))))
		  				 			(h (append (list (+ (car cur_pos) 2)) (list (+ (car (cdr cur_pos)) 5))))
		   							(i (append (list (+ (car cur_pos) 3)) (list (+ (car (cdr cur_pos)) 5))))
		   							(j (append (list (+ (car cur_pos) 4)) (list (+ (car (cdr cur_pos)) 5))))
		   							(k (append (list (+ (car cur_pos) 5)) (list (+ (car (cdr cur_pos)) 5)))))
		   							(append (append (append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   											(list d)) (list e)) (list f)) (list g))
														   											(list h)) (list i)) (list j)) (list k))))
		  ((equal? my-dir '(south)) (let* ((a (append (list (+ (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 5))))
		   							(b (append (list (+ (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 5))))
		   							(c (append (list (+ (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 5))))
		   							(d (append (list (+ (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 5))))
		   							(e (append (list (+ (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 5))))
		   							(f (append (list (car cur_pos)) (list (- (car (cdr cur_pos)) 5))))
		   							(g (append (list (- (car cur_pos) 1)) (list (- (car (cdr cur_pos)) 5))))
		  				 			(h (append (list (- (car cur_pos) 2)) (list (- (car (cdr cur_pos)) 5))))
		   							(i (append (list (- (car cur_pos) 3)) (list (- (car (cdr cur_pos)) 5))))
		   							(j (append (list (- (car cur_pos) 4)) (list (- (car (cdr cur_pos)) 5))))
		   							(k (append (list (- (car cur_pos) 5)) (list (- (car (cdr cur_pos)) 5)))))
		   							(append (append (append (append (append (append (append (append (append (append (list a) (list b)) (list c)) 
														   											(list d)) (list e)) (list f)) (list g))
														   											(list h)) (list i)) (list j)) (list k))))))


; Binder function to calculate the positions of each element in a given percept seen by the environment
(define (get-position cur_pos alist)
	(cond ((null? alist) '())
		  ((equal? (length alist) 3) (get-list1 cur_pos))
		  ((equal? (length alist) 5) (get-list2 cur_pos))
		  ((equal? (length alist) 7) (get-list3 cur_pos))
		  ((equal? (length alist) 9) (get-list4 cur_pos))
		  ((equal? (length alist) 11) (get-list5 cur_pos))))

; Function to dedupe already seen vegetation from the vegetation map
(define (dedupe clist)
	(cond ((null? clist) '())
		  ((member (car clist) (cdr clist)) (dedupe (cdr clist)))
		  (else (cons (car clist) (dedupe (cdr clist))))))

; Function to get the index in the percept list where the agent sees vegetation
(define (get-index percept idx)
	(cond ((null? percept) '())
		  ((not (list? (car percept))) (get-index (cdr percept) (+ 1 idx)))
		  ((equal? (list (car (car percept))) '(vegetation)) idx)
		  (else (get-index (cdr percept) (+ idx 1)))))

; Function to remove element at particular index from a list
(define (remove alist idx)
	(if (equal? idx 0) (cdr alist)
		(cons (car alist) (remove (cdr alist) (- idx 1)))))

; Function to extract the position of the vegetation seen by the agent. It calls the get-index() to
; get the index and then takes out the corresponding co-ordinates
(define (find-veg-pos percept percept-pos)
	(if (null? percept) '()
		(let ((pos (get-index percept 0)))
			(if (null? pos) '() 
				(append (list (list-ref percept-pos pos)) (find-veg-pos (remove percept pos) (remove percept-pos pos)))))))
			 
; Function to set the vegetation map on observing all the 5 percepts received by the agent.
(define (set-veg-map percepts p1 p2 p3 p4 p5)
	(let* ((veg-pos1 (find-veg-pos (car percepts) p1))
		   (veg-pos2 (find-veg-pos (car (cdr percepts)) p2))
		   (veg-pos3 (find-veg-pos (car (cdr (cdr percepts))) p3))
		   (veg-pos4 (find-veg-pos (car (cdr (cdr (cdr percepts)))) p4))
		   (veg-pos5 (find-veg-pos (car (cdr (cdr (cdr (cdr percepts))))) p5))
		   (all-veg-pos (append (append (append (append veg-pos1 veg-pos2) veg-pos3) veg-pos4) veg-pos5)))
		   (set! veg-map (dedupe (append veg-map all-veg-pos))))) 

; Initializing agent function. Currently contains nothing.
(define (initialize-agent)
        "OK")

; Function to check if there is a vegetation in front of the agent
(define (check-veg-infront)
	(cond ((equal? my-dir '(east)) (if (member (append (list (+ (car my-pos) 1)) (cdr my-pos)) veg-map) #t #f))
		  ((equal? my-dir '(west)) (if (member (append (list (- (car my-pos) 1)) (cdr my-pos)) veg-map) #t #f))
		  ((equal? my-dir '(north)) (if (member (append (list (car my-pos)) (list (+ (car (cdr my-pos)) 1))) veg-map) #t #f))
		  ((equal? my-dir '(south)) (if (member (append (list (car my-pos)) (list (- (car (cdr my-pos)) 1))) veg-map) #t #f))))

; Function to check if there is a boundary to the right of agent's current direction and position
(define (check-right-bound percepts)
	(if (null? percepts) #f
		(if (equal? (list-ref (car percepts) (+ (quotient (length (car percepts)) 2) 1)) 'barrier) #t #f)))

; Function to check if the agent was attacked by a predator in the previous turn
(define (check-pred-attack prev-events)
	(cond ((null? prev-events) #f)
		  ((equal? (list (car (car prev-events))) '(attacked-by)) #t)
		  (else (check-pred-attack (cdr prev-events)))))

; Function to get the next move of the agent, in case it has been attacked in the previous turn by a predator
(define (move-after-attack percepts)
	(cond ((and (equal? (list-ref (car percepts) 1) 'empty) 
				(equal? (list-ref (car (cdr percepts)) 2) 'empty)) "MOVE-AGGRESSIVE-2")
		  ((equal? (list-ref (car percepts) 1) 'empty) "MOVE-AGGRESSIVE-1")
		  ((equal? (list-ref (car percepts) 1) 'barrier) (if (check-right-bound percepts) "TURN-LEFT" "TURN-RIGHT"))
		  (else (if (check-right-bound percepts) "TURN-LEFT" "TURN-RIGHT")))) 

; Function to get the next move to reach the position "pos" from the current position, based on the agent's current direction
; The "pos" is in general the co-ordinates of the nearest vegetation. 
(define (get-next-move pos)
	(cond ((and (= (- (car pos) (car my-pos)) -1) (= (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-AROUND")
																								((equal? my-dir '(west)) "EAT-AGGRESSIVE")
																							   ((equal? my-dir '(north)) "TURN-LEFT")
																							   ((equal? my-dir '(south)) "TURN-RIGHT")))

	
		  ((and (= (- (car pos) (car my-pos)) 1) (= (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(west)) "TURN-AROUND")
																							   ((equal? my-dir '(east)) "EAT-AGGRESSIVE")
																							   ((equal? my-dir '(north)) "TURN-RIGHT")
																							   ((equal? my-dir '(south)) "TURN-LEFT")))

		  ((and (= (car pos) (car my-pos)) (= (- (car (cdr pos)) (car (cdr my-pos))) 1)) (cond ((equal? my-dir '(east)) "TURN-LEFT")
																							   ((equal? my-dir '(west)) "TURN-RIGHT")
																							   ((equal? my-dir '(north)) "EAT-AGGRESSIVE")
																							   ((equal? my-dir '(south)) "TURN-AROUND")))	
		
		  ((and (= (car pos) (car my-pos)) (= (- (car (cdr pos)) (car (cdr my-pos))) -1)) (cond ((equal? my-dir '(east)) "TURN-RIGHT")
																							   ((equal? my-dir '(west)) "TURN-LEFT")
																							   ((equal? my-dir '(north)) "TURN-AROUND")
																							   ((equal? my-dir '(south)) "EAT-AGGRESSIVE")))

		  ((and (< (car pos) (car my-pos)) (< (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-AROUND")
																						 ((equal? my-dir '(west)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(north)) "TURN-AROUND")
																						 ((equal? my-dir '(south)) "MOVE-AGGRESSIVE-1")))

		  ((and (< (car pos) (car my-pos)) (> (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-AROUND")
																						 ((equal? my-dir '(west)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(north)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(south)) "TURN-AROUND")))

		  ((and (< (car pos) (car my-pos)) (= (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-AROUND")
																						 ((equal? my-dir '(west)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(north)) "TURN-LEFT")
																						 ((equal? my-dir '(south)) "TURN-RIGHT")))
		
		  ((and (> (car pos) (car my-pos)) (< (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(west)) "TURN-AROUND")
																						 ((equal? my-dir '(north)) "TURN-AROUND")
																						 ((equal? my-dir '(south)) "MOVE-AGGRESSIVE-1")))
		
		  ((and (> (car pos) (car my-pos)) (> (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(west)) "TURN-AROUND")
																						 ((equal? my-dir '(north)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(south)) "TURN-AROUND")))
		
		  ((and (> (car pos) (car my-pos)) (= (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(west)) "TURN-AROUND")
																						 ((equal? my-dir '(north)) "TURN-RIGHT")
																						 ((equal? my-dir '(south)) "TURN-LEFT")))

		  ((and (= (car pos) (car my-pos)) (< (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-RIGHT")
																						 ((equal? my-dir '(west)) "TURN-LEFT")
																						 ((equal? my-dir '(north)) "TURN-AROUND")
																						 ((equal? my-dir '(south)) "MOVE-AGGRESSIVE-1")))

		  ((and (= (car pos) (car my-pos)) (> (car (cdr pos)) (car (cdr my-pos)))) (cond ((equal? my-dir '(east)) "TURN-LEFT")
																						 ((equal? my-dir '(west)) "TURN-RIGHT")
																						 ((equal? my-dir '(north)) "MOVE-AGGRESSIVE-1")
																						 ((equal? my-dir '(south)) "TURN-AROUND")))))

; Function calls the get-next-move function to get the next move towards the nearest vegetation.
; At each stage, function compares the distance of vegetation currently seen by the agent v/s the nearest vegetation already
; in the vegetation map and chooses the closest one. On choosing, it calls the get-next-move() to get the next move
; towards the selected vegetation
(define (go-to-veg percepts p1 p2 p3 p4 p5)
	(let ((veg-pos (find-veg-pos (car percepts) p1)))
		(if (not (null? veg-pos)) (let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
									(if (< (car np) (calc-distance (car veg-pos))) (get-next-move (car (cdr np))) 
																				   (get-next-move (car veg-pos))))
			(let ((veg-pos (find-veg-pos (car (cdr percepts)) p2)))
				(if (not (null? veg-pos)) (let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
                                    		(if (< (car np) (calc-distance (car veg-pos))) (get-next-move (car (cdr np)))
                                                                                   (get-next-move (car veg-pos))))
					(let ((veg-pos (find-veg-pos (car (cdr (cdr percepts))) p3)))
						(if (not (null? veg-pos)) (let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
                                    				(if (< (car np) (calc-distance (car veg-pos))) (get-next-move (car (cdr np)))
                                                                                   (get-next-move (car veg-pos))))
							(let ((veg-pos (find-veg-pos (car (cdr (cdr (cdr percepts)))) p4)))
								(if (not (null? veg-pos)) (let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
                                    						(if (< (car np) (calc-distance (car veg-pos))) (get-next-move (car (cdr np)))
                                                                                   (get-next-move (car veg-pos))))
									(if (not (null? p5)) (let ((veg-pos (find-veg-pos (car (cdr (cdr (cdr (cdr percepts))))) p5)))
										 					(let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
                                    							(if (< (car np) (calc-distance (car veg-pos))) (get-next-move (car (cdr np)))
                                                                                   (get-next-move (car veg-pos)))))
														 (let ((np (get-nearest-veg '(99999 (0 0)) veg-map)))
                                    							(get-next-move (car (cdr np))))))))))))))

; Function to check if there is any vegetation in agent's sight 
(define (veg-in-sight percepts p1 p2 p3 p4 p5)
	(let* ((veg-pos1 (find-veg-pos (car percepts) p1))
		   (veg-pos2 (find-veg-pos (car (cdr percepts)) p2))
		   (veg-pos3 (find-veg-pos (car (cdr (cdr percepts))) p3))
	       (veg-pos4 (find-veg-pos (car (cdr (cdr (cdr percepts)))) p4)))
		  (if (null? p5) (let ((all-veg (append (append (append veg-pos1 veg-pos2) veg-pos3) veg-pos4)))
								(if (null? all-veg) #f #t))
		   				 (begin 
							(let* ((veg-pos5 (find-veg-pos (car (cdr (cdr (cdr (cdr percepts))))) p5))
		    					   (all-veg (append (append (append (append veg-pos1 veg-pos2) veg-pos3) veg-pos4) veg-pos5)))
		   						   (if (null? all-veg) #f #t))))))

; Function to calculate the distance between a point (in general, it is a vegetation) with the current position
(define (calc-distance veg-pt)
	(sqrt (+ (expt (- (car my-pos) (car veg-pt)) 2)
			 (expt (- (car (cdr my-pos)) (car (cdr veg-pt))) 2))))

; Function to get the co-ordinates of the position in front of the agent, based on the direction it is facing
(define (get-front-pos)
	(cond ((equal? my-dir '(east)) (append (list (+ (car my-pos) 1)) (cdr my-pos)))
		  ((equal? my-dir '(west)) (append (list (- (car my-pos) 1)) (cdr my-pos)))
		  ((equal? my-dir '(north)) (append (list (car my-pos)) (list (+ (car (cdr my-pos)) 1))))
		  ((equal? my-dir '(south)) (append (list (car my-pos)) (list (- (car (cdr my-pos)) 1))))))

; Function to get the co-ordinates of vegetation in the vegetation map nearest to the agent
(define (get-nearest-veg nearest-point all-veg)
	(if (null? all-veg) nearest-point
		(if (equal? (car all-veg) (get-front-pos)) (get-nearest-veg nearest-point (cdr all-veg))
		(let ((pt (calc-distance (car all-veg))))
			 (if (< pt (car nearest-point)) (let ((np (append (list pt) (list (car all-veg)))))
												 (get-nearest-veg np (cdr all-veg)))
				 (get-nearest-veg nearest-point (cdr all-veg)))))))

; Function to get the move towards the nearest vegetation from agent's current position.
; This function is called in two cases :
; 	1. When the vegetation map is empty - default command of MOVE-AGGRESSIVE-2 is issued
; 	2. When the agent cannot see any vegetation at the moment, has not been attacked by a predator and
; 	   does not have a barrier in front of it. In this case, it gets the next move towards the nearest
; 	   vegetation.	
(define (find-veg percepts)
	(if (null? veg-map) "MOVE-AGGRESSIVE-2"
		(let* ((nearest-point '(99999 (0 0)))
		   (veg-pos (get-nearest-veg nearest-point veg-map)))
			(if (equal? veg-pos nearest-point) (if (check-right-bound percepts) "TURN-LEFT" "TURN-RIGHT")
				(get-next-move (car (cdr veg-pos)))))))

; Function to check if the agent cannot move straight ahead  
(define (check-barrier percepts)
	(if (equal? (list-ref (car percepts) 1) 'empty) #f #t))					

; Function that checks conditions and decides what move to make. Currently main conditions checked are :
; 	1. If there is a vegetation in front of the agent, eat it. In case the bloom cycle of the vegetation is over
; 			then, find the other closest vegetation in sight. If there isn't any vegetation, check boundary and turn
; 	2. If the agent is attacked in the previous turn, move accordingly
; 	3. If there is a vegetation is sight, get the move to go towards it.
; 	4. If none of the above is true, find the closes vegetation in the vegetation map and move towards it. If there is no
; 			vegetation in the vegetation map, it moves ahead.
;
; This function is called by the choose-action()
(define (select-action prev-events percepts p1 p2 p3 p4 p5)
	(cond ((check-veg-infront) (cond ((check-pred-attack prev-events) (move-after-attack percepts))
									 ((not (equal? (list-ref (car (cdr (car percepts))) 2) 0)) "EAT-AGGRESSIVE") 
									 (else (if (veg-in-sight (cdr percepts) p2 p3 p4 p5 '()) (go-to-veg (cdr percepts) p2 p3 p4 p5 '()) 
																					  		 (if (check-right-bound percepts) "TURN-LEFT" "TURN-RIGHT")))))
		  ((check-pred-attack prev-events) (move-after-attack percepts))
		  ((check-barrier percepts) (if (check-right-bound percepts) "TURN-LEFT" "TURN-RIGHT"))
		  ((veg-in-sight percepts p1 p2 p3 p4 p5) (go-to-veg percepts p1 p2 p3 p4 p5))
		  (else (find-veg percepts)))) 

; Helper function to change the current position based on the previous move
(define (change-pos amount)
	(cond ((equal? my-dir '(east)) (append (list (+ (car my-pos) amount)) (cdr my-pos)))
		  ((equal? my-dir '(west)) (append (list (- (car my-pos) amount)) (cdr my-pos)))
		  ((equal? my-dir '(north)) (append (list (car my-pos)) (list (+ (car (cdr my-pos)) amount))))
		  ((equal? my-dir '(south)) (append (list (car my-pos)) (list (- (car (cdr my-pos)) amount))))))

; Function to chance the current position based on the previous  move
(define (set-my-pos prev-events)
	(cond ((null? prev-events) my-pos)
		  ((equal? (list (car (car prev-events))) '(moved)) (change-pos (car (cdr (car prev-events)))))
		  (else (set-my-pos (cdr prev-events)))))

; Function to get the new direction based on the previous TURN command
(define (get-new-dir new-dir)
	(cond ((equal? new-dir "TURN-LEFT") (cond ((equal? my-dir '(east)) '(north))
											  ((equal? my-dir '(west)) '(south))
											  ((equal? my-dir '(north)) '(west))
											  ((equal? my-dir '(south)) '(east))))

	 	  ((equal? new-dir "TURN-RIGHT") (cond ((equal? my-dir '(east)) '(south))
											  ((equal? my-dir '(west)) '(north))
											  ((equal? my-dir '(north)) '(east))
											  ((equal? my-dir '(south)) '(west))))

	 	  ((equal? new-dir "TURN-AROUND") (cond ((equal? my-dir '(east)) '(west))
											  ((equal? my-dir '(west)) '(east))
											  ((equal? my-dir '(north)) '(south))
											  ((equal? my-dir '(south)) '(north))))))

; Function to set the current direction to the new direction it is facing
(define (set-my-dir new-dir)
	(if (not (equal? new-dir my-dir)) (set! my-dir new-dir)))

; Main function, which is the API for the environment simulation. Returns the agent's next move based on 
; the percepts the agents sees.
(define (choose-action current-energy previous-events percepts)
	(set! my-pos (set-my-pos previous-events))
	(let* ((p1 (get-position my-pos (car percepts)))
		   (p2 (get-position my-pos	(car (cdr percepts))))
		   (p3 (get-position my-pos (car (cdr (cdr percepts)))))
		   (p4 (get-position my-pos (car (cdr (cdr (cdr percepts))))))
		   (p5 (get-position my-pos (car (cdr (cdr (cdr (cdr percepts))))))))
		  (set-veg-map percepts p1 p2 p3 p4 p5)
		  (let ((actuator (select-action previous-events percepts p1 p2 p3 p4 p5)))
			   (cond ((or (equal? actuator "TURN-LEFT") 
						  (equal? actuator "TURN-RIGHT") 
						  (equal? actuator "TURN-AROUND")) (let ((new-dir (get-new-dir actuator)))
																 (set-my-dir new-dir)
																 actuator))
					 (else actuator)))))
		    
