#lang racket

(provide code-walker)

(define (code-walker code)
  
  ;(define program-structure (syntax-e code))
  #;(define program-aux program-structure)
  (define final-list null)
  (displayln 
   (let extract-syntax ((program-structure (syntax-e code)))
     (display "ENTREI, null? ")
     (displayln (null? program-structure))
     (unless (null? program-structure)
       (let loop ((program-aux program-structure)
                  (current (car program-structure))
                  (next (cdr program-structure)))
         (if (null? next)
             (list)
             (begin 
               (displayln current)
               (display "PAIR? ")
               (displayln (pair? (cdr program-aux)))
               ;(displayln (cdr program-aux))
               (display "Syntax? ")
               (displayln (syntax? (cdr program-aux)))
               (displayln " END ITERATION ")
               (if (syntax? next)
                   (begin
                     ;(displayln (vector? current)) ;#f
                     ;(displayln (box? current)) ;#f
                     ;(displayln (syntax? current)) #t
                     ;(displayln (syntax-e current))
                     (displayln " SYNTAX ")
                     (display "DATUM ")
                     (displayln (syntax->datum current))
                     (extract-syntax (syntax-e current))
                     #;(when (null? (syntax-e next))
                         (begin 
                           (display "DATUM ")
                           (displayln (syntax->datum current))
                           (extract-syntax (syntax-e current)))))
                   (begin
                     (displayln "NEXT PAIR")
                     (cons current
                           (loop (cdr program-aux)
                                 (cadr program-aux)
                                 (cddr program-aux)))))))))
     (displayln "It is Null")))
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
  
