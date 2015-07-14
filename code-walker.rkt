#lang racket

(provide code-walker)

(define (code-walker code)
  
  ; #<syntax module> I want this.
  ;(define program-structure (syntax-e code))
  #;(define program-aux program-structure)
  (define final-list null)
  (define deep #f)
  (displayln 
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
  