#lang racket

(provide code-walker)

(define (code-walker code)
  
  ; #<syntax module> I want this.
  ;(define program-structure (syntax-e code))
  #;(define program-aux program-structure)
  (define final-list null)
  (define (syntax-walker program current next previous)
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
           (if (pair? (cdr program)) ;bug here!
               (loop (cdr program) (cadr program) (cddr program) current)
               (begin
                 (displayln "$$$$$$$$$$$$$$$ This should not happen $$$$$$$$$$$$$$$")
                 (display "$$$$$$$$$$$$$$$")
                 (displayln (car program)); do stuff
                 (display "$$$$$$$$$$$$$$$")
                 (displayln (syntax? (car program)))
                 )
               )]
        [(and (syntax? next) (not (null? (syntax-e next)))) (loop (syntax-e next) (car (syntax-e next)) (cdr (syntax-e next)) current)]
        [else (displayln "$$$$$$$$$$$$$$$ Case not supported, aka You found a BUG!! $$$$$$$$$$$$$$$")]))
    #;(displayln (syntax->datum program)))
  
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
  (go-to-syntax (syntax-e code))
  #;(displayln 
     (let extract-syntax ((program-structure (syntax-e code))) ;goes down 1 level
       (display "ENTREI, null? ")
       (displayln (null? program-structure))
       (unless (null? program-structure)
         (let loop ((program-aux program-structure) ;walks trought a level.
                    (current (car program-structure))
                    (next (cdr program-structure)))
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
                 (displayln " END ITERATION ")
                 (display "Check if current is syntax: ")
                 (displayln (syntax? current))
                 
                 (when (and (syntax? next) (syntax? current) deep) ;it has to be the next, only carrying about the deep flag. #FIXTHIS
                   (begin
                     (display "DEEP ENOUGH!?!          ")
                     (displayln next)
                     (extract-syntax (syntax-e next))
                     )
                   )
                 (if (syntax? next) 
                     (begin
                       (set! deep #t)
                       (displayln " SYNTAX ")
                       (display "DATUM ")
                       (displayln (syntax->datum current))
                       (extract-syntax (syntax-e current)))
                     (begin
                       (displayln "NEXT PAIR")
                       (cons current
                             (loop (cdr program-aux)
                                   (cadr program-aux)
                                   (cddr program-aux)))))))))))
  (displayln "END FILE")
  
  
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
  )
