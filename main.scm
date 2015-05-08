; Assignment 5 - Implementing Genetic Algorithm to search a global maxima in a 7 dimensional landscape.

; There are 6 variables and each have a value between 1 and 20 inclusive. The program provides an interface
; called "search-ga" which accepts a function name as a parameter. This is the evaluating function
; to calculate the fitness value of the members of the population. In this file, "my-eval-fn" is the fitness
; function used. Our aim is to maximize the fitness function value. 
;
; The algorithm flow involves : 
; 	1) Generate an initial population. 
; 	2) Calculate the fitness value function of each member, by calling "my-eval-fn". 
; 	3) Using the tournament selection, it selects two members, usually with high fitness value, for mating.
; 	4) Step 3. is repeated for the entire population.
; 	5) Each chosen pair from step 4. is mated by choosing a crossover point at random, giving new members/offsprings.
; 	6) The newly generated members are mutated. This is to promote exploration and avoid getting stuck in a local maxima. 
; 	7) This new population is then re-evaluated by calling the "my-eval-fn" in Step 2.
;	
; Optimizations done :
; 	1) Initial population size = 50
; 	2) Size of smaller population from which a pair is selected to mate = 25, using Tournament Selection.
;	3) Choosing cross-over point = random
;	4) Mutation Rate = 10
;
; Total time taken to display the result = 15 seconds.
;
; The above cycle goes on untill the "my-eval-fn" is called a maximum of 50,000 times. 
; In the end, the best member giving the highest fitness value, across all generations, is returned back.
;
; Created by Anand Goyal, April 2015. © Anand Goyal, April 2015. Only for academic purposes.


; Helper function for the fitness function. Returns the nth element from a list

(define (get-nth index alist)
	(cond ((= 1 index) (car alist))
		  (#t (get-nth (- index 1) (cdr alist)))))

; The fitness function to calculate the fitness value of member.

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

; Generates a member of size = 6 for the initial population, each having values within the domain 1-20.

(define (generate-member size domain)
	(if (zero? size) '()
		(append (list (+ (random domain) 1)) (generate-member (- size 1) domain))))

; Generates the initial population of size = size.

(define (generate-pop size num domain)
	(if (zero? num) '()
		(append (list (generate-member size domain)) (generate-pop size (- num 1) domain))))	 

; Wrapper function to call the fitness function to calculate fitness value for the entire population.
; It appends the fitness value to the front of the list.

(define (evaluate-pop pop eval-fn)
	(if (null? pop) '()
		(if (= (length (car pop)) 6) 
			(let ((fit-val (eval-fn (car pop))))
				(append (list (cons fit-val (car pop))) (evaluate-pop (cdr pop) eval-fn)))
			(let ((fit-val (eval-fn (cdr (car pop)))))
				(append (list (cons fit-val (cdr (car pop)))) (evaluate-pop (cdr pop) eval-fn)))))) 

; Helper function to implment shuffle function. It removes the element at index = idx.

(define (list-remove alist idx)
	(if (zero? idx) (cdr alist)
		(append (list (car alist)) (list-remove (cdr alist) (- idx 1)))))

; Function to shuffle a given list. Helps in picking elements at random.

(define (shuffle pop)
	(if (null? pop) '()
		(let ((index (random (length pop))))
			(append (list (list-ref pop index)) 
				(shuffle (list-remove pop index))))))

; Function to take n = size elements from the front of the list.

(define (take alist size)
	(if (null? alist) '()
		(if (zero? size) '()
			(append (list (car alist)) (take (cdr alist) (- size 1))))))

; Helper function to implement sorting a list (in this case, a list of lists) by the first variable of the member.

(define (compare a b)
	(> (car a) (car b)))

; Function to implement the choosing of 2 members to crossover. This method is called tournament selection. 
; Tournament selection is done by:
; 	1) Shuffle the population
; 	2) Take some elements from the shuffled list (best results found @ 25 = pop-size/2)
; 	3) Sort the elements selected in step two, in decreasing order of their fitness values
; 	4) Take first two elements from the list. These are the two elements selected to mate

(define (select-two pop size)
	(if (null? pop) '()
		(take (sort (take (shuffle pop) size) compare) 2)))

; Function to mate the two selected members of the population. This is done by selecting a cross-over point 
; at random. The two members are the crossed-over or mated across that point to produce new off-springs.

(define (cross-over mem1 mem2)
	(if (or (null? mem1) (null? mem2)) '()
		(let ((point (random (min (length mem1) (length mem2)))))
			(append (list (append (take mem1 (+ point 1)) (list-tail mem2 (+ point 1))))
					(list (append (take mem2 (+ point 1)) (list-tail mem1 (+ point 1))))))))	

; Function to mate the entire population. Initially two members are selected using tournament selection. 
; Then the selected members are mated by calling the cross-over function. These steps are repeated untill
; new members are generated to replace the entire previous population. 

(define (mate-pop eval-pop pop-size select-size)
	(if (null? eval-pop) '()
		(if (zero? pop-size) '()
			(let ((members-to-mate (select-two eval-pop select-size)))
				(let ((children (cross-over (car members-to-mate) (car (cdr members-to-mate)))))
					(append children (mate-pop eval-pop (- pop-size 2) select-size))))))) 
	
; Function to mutate a member of the population. The mutation is done according the mutation rate.
; The mutation rate controls if the variable in the member has to be mutated or not. The fitness value
; came best for mutation rate = 10.

(define (mutate-mem mem domain mutation-rate)
	(if (null? mem) '()
		(if (< (random domain) mutation-rate) 
			(append (list (+ (random domain) 1)) (mutate-mem (cdr mem) domain mutation-rate))
			(append (list (car mem)) (mutate-mem (cdr mem) domain mutation-rate)))))

; Function to mutate the entire population by calling the mutate-mem function for each member.

(define (mutate-pop eval-pop domain mutation-rate)
	(if (null? eval-pop) '()
		(append (list (cons (car (car eval-pop)) (mutate-mem (cdr (car eval-pop)) domain mutation-rate))) (mutate-pop (cdr eval-pop) domain mutation-rate))))

; Function to get the best member, based on the fitness function value, for the current population.

(define (get-best-member pop)
	(if (null? pop) '()
		(car (sort pop compare))))

; Function to keep track of the best member, based on fitness function value, across all the generations.

(define (set-global-best cur-best best-mem)
	(if (null? best-mem) cur-best
		(if (> (car cur-best) (car best-mem)) cur-best best-mem))) 

; Helper function to calculate the average fitness function value for the current generation.
; Function to calculate the sum of all fitness values in a population.

(define (calc-fitness-sum pop)
	(if (null? pop) 0
		(+ (car (car pop)) (calc-fitness-sum (cdr pop)))))

; Function to calculate the average of current population.

(define (calc-fitness-avg pop size)
	(if (null? pop) 0
		(/ (/ (calc-fitness-sum pop) size) 1.0)))

; Main function to create all the generations and apply the Genetic Algorithm. It receives the following parameters :
; 	1) Number of generations it has to create -> based on the initial population size and total times the fitness function can be called. (Optimal = 50,000/50)
; 	2) Initial population -> generated by the search-ga function.
; 	3) Population size -> Optimal = 50
; 	4) Domain -> range of values for the variables (1 to 20)
; 	5) Select-size -> The size of smaller population, from which a pair is selected to mate. (Optimal = 25 or pop-size/2).
; 	6) Mutation-rate -> The value below which mutation should take place. (Optimal = 10)
; 	7) Best member -> The best member across all generations seen till now.
; 	8) My-eval-function -> Fitness function, supplied as an argument, to calculate the fitness value of the population in each generation.
; 	
; The function returns the best member across all the generations, untill the fitness function has been called a maximum of 50,000 times.	

(define (create-new-gen gens init-pop pop-size domain select-size mutation-rate best-mem my-eval-func)
	(if (zero? gens) best-mem 
		(let ((eval-pop (evaluate-pop init-pop my-eval-func)))
			(let ((avg-fit (calc-fitness-avg eval-pop pop-size)))
				(let ((cur-best (get-best-member eval-pop)))
					(let ((global-best (set-global-best cur-best best-mem)))
						(let ((offsprings (mate-pop eval-pop pop-size select-size)))
							(let ((mutated-pop (mutate-pop offsprings domain mutation-rate)))
								(create-new-gen (- gens 1) mutated-pop pop-size domain select-size mutation-rate global-best my-eval-func)))))))))

; The main interface function. It accepts the fitness function name as an argument which is used to calculate the fitness values for each member in the population.
; The function generates the initial population and passes it to the "create-new-gens", along with other arguments.
; Function finally displays the best member, with values of each variable that gave the best fitness function value.
; The function also displays the corresponding fitness function value.

(define (search-ga my-eval-func)
	(let ((pop (generate-pop 6 50 20)))
		(let ((gens (/ 50000 50)))
			(let ((best (create-new-gen gens pop 50 20 25 10 '() my-eval-func)))
				(display '(best mem :)) (display (cdr best)) (newline) 
				(display '(fitness value :)) (display (car best)) (newline)))))
