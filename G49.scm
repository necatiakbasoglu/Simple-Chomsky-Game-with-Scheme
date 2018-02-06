#lang racket



(define printList
  (lambda (lis)
    (cond((null? lis)(display " ") )
         (else (display (car lis))(newline) (printList (cdr lis)))
         )
    ))

(define add
  (lambda (liste n yeni)
  (cond((= n 0) yeni)
       (else(add (cdr liste) (- n 1) (cons (car liste) yeni))))
  ))

(define reverseList
  (lambda (liste yeni)
    (cond((null? liste) yeni)
         (else(reverseList (cdr liste) (cons (car liste) yeni) )))
  )
)

(define create(lambda (liste n)
    (cond((= n 0) liste)
         (else(create (cons "-" liste) (- n 1))))
  ))

(define (createEmptyList n)
  (create '() n)
)

(define eat(lambda (liste column)
          (reverseList (add liste (- (length liste) (+(- (length liste) column) 1) ) '()) (createEmptyList (+(- (length liste) column) 1)))
         ))

(define delete(lambda (liste row column yeni)
  (cond((null? liste) yeni)
       ((< row 2) (delete (cdr liste) (- row 1) column (cons (eat (car liste) column) yeni)) )
       (else (delete (cdr liste) (- row 1) column (cons (car liste) yeni))
             ))))

(define EAT(lambda (board-list row column)
             (reverseList(delete board-list row column '()) '())))





(define check-end-game
  (lambda (li)
    (cond ((null? li) true)
          ((string=? (caar li) "x") #f)
          ((null?  (cdr (car li))) (check-end-game (cdr li)))
          (else(check-end-game (cons (cdr (car li) ) (cdr li))))
          )))

(define isE
  (lambda (n m liste)
    (isE-iter n 1 m 1 liste)
      ))

(define isE-iter
  (lambda (n n1 m m1 liste)
    (cond ((= n1 n) (other m m1 (car liste)))
          (else ( isE-iter n (+ n1 1) m m1 (cdr liste) ) )
    )))

(define other
  (lambda (m m1 liste)
    (cond ((equal? m m1) (cond ((equal? (car liste) "x") false)
                          (else true)))
          (else ( other m (+ m1 1) (cdr liste)))
          )
    ))


(define computer
  (lambda (x m list1 q)
    (cond ((> m x) (cons x (list q)))
          ((not (equal? (isE m q list1) (isE q m list1)))
                 (cond ((isE m q list1) (cons q (list m)) )
                       (else (cons m (list q)))
                       ))

          
      ((and (equal? #t (isE m q list1)) (equal? #t (isE q m list1)))
       (cond((= q 1) (append (list (- m 1)) (list q)))
            ((isE q q list1) (computer x m list1 (- q 1)))
            (else(append (list q) (list q)))))
       
       
                 
          (else (computer x (+ m 1) list1 q)  
                ))
    ))

(define create-board
  (lambda (n m)
    (create-board-iter n m 0 '())))

(define create-board-iter
  (lambda (n m x liste)
    (cond ((= n x) liste)
          ((create-board-iter n m (+ 1 x) (cons (create-row m 0 '()) liste) ))
          )
    ))

(define create-row
  (lambda (n m liste)
    (cond ((= n m) liste)
         ( (create-row n (+ 1 m) (cons "x" liste)))
          )))


(define decision (lambda (board humanRow humanColumn)
                   (cond((> (length board) (length (car board))) (computer (length (car board)) 2 board (cond((< humanRow humanColumn)humanRow)(else humanColumn))) )
                        (else(computer (length board) 2 board (cond((< humanRow humanColumn)humanRow)(else humanColumn)))))))


(define (user-input prompt)
    (display prompt)
    (read))

(define play(lambda (board turn checklist)
              (printList board)
              (cond((check-end-game board) (cond((= turn 0) (display "HUMAN WON") )((display "COMPUTER WON "))))
                   ((= turn 0) (play board 2 (list (user-input "Enter Row: \n") (user-input "Enter Column: \n"))))
                   ((= turn 2)(display "After human played \n") (play(EAT board (car checklist) (cadr checklist)) 1 checklist ))
                  ((display "After computer played \n")
                   (play (EAT board (car (decision board (car checklist) (cadr checklist))) (cadr (decision board (car checklist) (cadr checklist)))) 0 checklist))
                    )
              ))

(display "First row first column with index (1 , 1) cookie is poisonous. \n")
(display "You should create a board first so that you have to enter row and column dimensions!! \n")
(play (create-board (user-input "Enter row dimension \n") (user-input "Enter column dimension  \n")) 0 '() )
