#lang racket

(provide code-walker)
(define (walk-trought program current next previous)
  (if (null? next)
      (list)
      (begin
        (display "Current: ")
        (displayln current)
        (display "PAIR? ")
        (displayln (pair? next))
        ;(displayln (cdr program-aux))
        (display "Syntax? ")
        (displayln (syntax? next))
        (cond
          [(syntax? next) ;Found syntax, go to there.
           (begin
             (displayln " SYNTAX ")
             (display "DATUM ")
             (displayln (syntax->datum current))
             ;(go-to-syntax (syntax-e current))
             (define aux (syntax-e current))
             (displayln (syntax->datum current))
             (syntax-walker aux (car aux) (cdr aux) null )
             )]
          [else 
           (begin
             (displayln "NEXT PAIR")
             (cons current
                   (walk-trought (cdr program)
                                 (cadr program)
                                 (cddr program) 
                                 null)))]))))
(define (go-to-syntax program-structure)
  (display "[go-to-syntax] program-structure null? ")
  (displayln (null? program-structure))
  (unless (null? program-structure)
    (walk-trought program-structure (car program-structure) (cdr program-structure) null ))
  (displayln "!!!!!!!!!!!! END OF FILE !!!!!!!!!!!!")
  )

(define (syntax-walker program current next previous)
  ;previous is not working.
  (define iteration 0)
  (define syntax-list (list))
  (displayln "Syntax-walker in the HOUSE!!")
  (let loop ((program program)
             (current current)
             (next next)
             (previous previous))
    (display "[Syntax-walker]  Current: ")
    (displayln current)
    (display "[Syntax-walker]  PAIR? ")
    (displayln (pair? next))
    (display "[Syntax-walker]  Syntax Next? ")
    (displayln (syntax? next))
    ;Need a test to know if it is a syntax node of the program.
    (display "[Syntax-walker]  Syntax Current ")
    (displayln (syntax? current))
    
    #|If #t must go and visit each syntax node of the program
      Have a way if it is not true to go back in the code.|#
    
    (cond
      [(pair? next) 
       (if (pair? (cdr program))
           (begin ;hack!
             (when (> iteration 2)
               (set! syntax-list (cons syntax-list current)))
             (loop (cdr program) (cadr program) (cddr program) current)
             )
           (begin
             (displayln "!!!!!!!!!!!!!!!! This should not happen !!!!!!!!!!!!!!!!")
             (display "!!!!!!!!!!!!!!!!")
             (displayln (car program)); do stuff
             (display "!!!!!!!!!!!!!!!!")
             (displayln (syntax? (car program)))
             )
           )]
      [(and (syntax? next) (not (null? (syntax-e next)))) (loop (syntax-e next) (car (syntax-e next)) (cdr (syntax-e next)) current)]
      [(not (or (pair? next) (syntax? next))) (displayln "$$$$$$$$$$$$$$$ End File $$$$$$$$$$$$$$$")]
      [else (displayln "!!!!!!!!!!!!!!!! Case not Supported !!!!!!!!!!!!!!!!")])) ;this happens on a black file. Find out why.
  (explore-nodes syntax-list)
  #;(displayln (syntax->datum program)))

#|explore-nodes receives a syntax-list of the first level of the program, avoiding the need to explore every single node of the program.
  
 |#
(define (explore-nodes syntax-list)
  (define level 0)
  (define offset 0)
  (define previous-node null)
  (define node null)
  (define next-node null)
  #|Level increases when entering a syntax object (syntax-e)
    If previous-node is null and level is > 0 you go down a level to the corresponding offset
    If next-node is null It may assume that you finish the syntax object, you can go
 to the next syntax object, by going up a level and increasing the offset. If that is not possible it ends the search.
    Selecting a syntax object increases the level.
    Going to the next syntax object increases the offset.
   |#
  (define (select-syntax-object)
    (displayln "Selecting syntax")
    (set! level (add1 level)))
  (define (next-syntax-object)
    ; or return void or error, whatever. then go up a level
    (displayln "Next Syntax")
    (set! offset (add1 offset)))
  (define (previous-syntax-object)
    ;or sub1 in the level and return void/error
    (displayln "Previous syntax")
    (set! offset (sub1 offset)))
  (define (next-node)
    ;check this out
    (displayln "next node")
    (set! level (sub1 level))
    (set! offset (add1 offset)))
  (define (previous-node)
    ;figure out what to do with this.
    (displayln "previous node"))
  (define (compare-syntax current syntax-wanted)
    (display "Checking syntax: Current ")
    (display current)
    (display "  Wanted ")
    (displayln syntax-wanted)
    )
  (displayln "EXPLORING LIST")
  
  )

(define (code-walker code)
  #;(define program-aux program-structure)
  (define final-list null)
  (go-to-syntax (syntax-e code))
  (displayln "END FILE"))


#|(displayln  program-structure)
  #;(for ((sexp (in-list  program-structure)))
      (displayln sexp))
  #;(displayln (syntax? code))
  #;(displayln (identifier? code))
  #;(displayln (syntax-source code))
  #;(displayln (syntax-line code))
  #;(displayln (syntax-column code))
  
  #;(displayln code) ;in theory this is the syntax object of the program
  ;test One layer
  #;(displayln (syntax-e code))
  ;test EVERYTHING
  #;(displayln (syntax->datum code)) |#

