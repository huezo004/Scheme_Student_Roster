 (define (remove-sub lst  item )
  (cond ((null? lst) '() )
	((equal? (car lst) item) (cdr lst))
	(else (cons (car lst)(remove-sub (cdr lst) item)))
	))

(define (smallest lst  item)
  (cond ( (null? lst) item)
	((<(string->number(car(car lst))) (string->number(car item)))  ;cambia la orden
	(smallest (cdr lst) (car lst)))
	(else (smallest (cdr lst) item))   ; seguir pasando el mismo

	))

(define (sort lst)
  (cond ((null? lst) '() )
	(else(cons(smallest lst (car lst))
		    (sort(remove-sub lst(smallest lst (car lst))))))
))

(define display-student
(lambda (lst item)
(cond 
 ((null? lst)
  (begin
  (display "\n\tStudent")
  (display item)
  (display "is not in the roster.\n"))
)
 ((equal? item (car (car lst)))
 (display-info (car lst)))

 ((equal? item  (car (cdr (car lst))))
  (display-info (car lst)))

 (else
 (display-student (cdr lst) item))
 )))

(define print
  (lambda (lst)
  (cond ((null? lst) '())
(else
    
(begin 
(display "ID: ")
(display (car(car lst)))
(newline)
(display "Name: ")
(display  (cadr(car lst)))
(newline)
(display "Grade:")
(display (caddr(car lst)))
(newline)
(print (cdr lst))

))

)))


(define ano-read-2-items
  (lambda (n lst)
     (cond ((= n 0) (begin
		     (display "Student ID: ")
		     (ano-read-2-items 1 (list (read-line)))
		     ))
	  ((= n 1)  (begin
		     (display "Student name: ")
		     (ano-read-2-items 2 (list (car lst)(read-line)))
	             ))
          ((= n 2)  (begin
		    (display "Grade:")
		    (list (car lst) (cadr lst) (read-line))
	            ))
    )
  )
 )

(define remove 
  (lambda (lst item)
    (cond
     ((null? lst)
       (begin 
       (display "Student")
       (display item)
       (display "is not in Roster\n")
       ;('()) not empty stu. not there?
      ))
   ((equal? (car (car lst)) item)
   (begin
   (display "ID:\n")
   (display item)  
   (display "removed.\n")
   (cdr lst))) 
        
  ((equal? (cadr (car lst)) item)
   (begin
   (display "Student:\n")
   (display item)  
   (display "removed.\n")
   (cdr lst))) 
 
 (else 
  (cons (car lst) (remove (cdr lst) item )))
)))


(define performtask
  (lambda (n roster)
    (cond ((= n 0) (begin
		     (display "\n\tRoster reset (now empty).\n")
		     (menu '())
		     ))

          ((= n 1)(begin	    
		    
		     (display "\n\tEnter file name: ")
		     (let ((filename (open-input-file (read-line))))
		     (let ((fileroster (read filename)))
		     (close-input-port fileroster)
		     (display "\tRoster loaded from file.\n")
		     (menu fileroster)
		))))
	 ((= n 2) (begin
		    (display "\n\tEnter file name: ")
		    (let (( wfile (open-output-file (read-line))))
		      (write roster wfile)
		      (close-output-port wfile)
		      )
		    (display "\tRoster written in file.\n")
		    (menu roster)
		    ))
	 ((= n 3) (begin
		    (sort roster)
		    (print roster)
		   (newline)
		   (menu roster)
		    ))

	 ((= n 4) (begin
		    
		    (display "Enter student Name or ID:")
		    (display-student roster (read-line))
		    (menu roster)
	  ))
         
         ((= n 6)(begin
		(display roster)   
                (display "Enter a student name or ID to remove:")
		(newline)
		(menu(remove roster (read-line)))
                ))

	((= n 5)(begin
		    (display "\n\tAdd a student to roster\n")
		    (newline)	      
		    (menu(cons(ano-read-2-items 0 '()) roster))
		    ))
	
         ((= n 7) (begin
	       (display "\nGoodbye!!\n")
	       (exit) 
	      ))

	  (else (begin
		  (display "\n\ttask no. ")
		  (display n)
		  (display " does not exist.\n\n")
		  (menu roster)
		  )
		)
	  )
    )
)  

(define menu
  (lambda (roster)
    (begin
      (display "\t============================\n")
      (display "\t   MENU\n")
      (display "\t============================\n")
      (display "\t0. Reset roster\n")
      (display "\t1. Load roster from file\n")
      (display "\t2. Store roste to file\n")
      (display "\t3. Display roster sorted by ID\n")
      (display "\t4. Display student info\n")
      (display "\t5. Add a student to roster\n")
      (display "\t6. Remove a student from roster\n")
      (display "\t7. Exit\n")
      (display "\tEnter your choice:\n") 
      (performtask (read) roster)
      )
    )
  )
