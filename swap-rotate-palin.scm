;Assignment 1 by Anand Goyal, 4/11/15
;Artificial Intelligence under Professor Conner
;Spring 2015

;Question 1 to swap the adjacent elements in a list. Function takes a list as an argument
(define (swap-adjacent alist)
	(if (null? alist) alist
		(if (null? (cdr alist)) alist
			(append (list (car (cdr alist)) (car alist)) (swap-adjacent (cdr (cdr alist))))))) 

;Question 2 to rotate left a list by given positions. Function takes number of positions and list as arguments
(define (rotate-left pos alist)
	(if (null? alist) alist
		(if (< pos 1) alist 
			(rotate-left (- pos 1) (append (cdr alist) (list (car alist)))))))

;Question 3 to check if the entered string is a palindrome or not
;palindrome() calls the revlist() to reverse the entered string and then compares it to the original string
(define (revlist lis)
	(if (null? lis) '()
		(append (revlist (cdr lis)) (list (car lis)))))

(define (palindrome alist)
	(if (null? alist) #t
		(equal? alist (revlist alist))))

