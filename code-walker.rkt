#lang racket

(provide code-walker)

(define (code-walker code)
  
  (define program-list (syntax-e code))
  (displayln program-list)
  (for ((sexp (in-list program-list)))
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
  #;(displayln (syntax->datum code))
  ;Test list, readable information
  #;(define testlist (syntax->list code))
  #;(for ((sexp (in-list testlist)))
      (displayln sexp)))
 