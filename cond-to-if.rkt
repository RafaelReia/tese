#lang racket
(require syntax/parse)
(define cond-to-if #'(cond [(< 1 2) #t]
                            [(< 2 3) #t]
                            [(< 4 5) #t]
                            [else #t]))
(define (write-if conds thens last-else)
  (define aux-result null)
  (define result null)
  (define (write-aux conds thens)
    (unless (null? conds)
      (begin
        ;(displayln (syntax? conds))
        (set! aux-result (cons #`(#,(car conds) #,(car thens)) aux-result))
        (write-aux (cdr conds) (cdr thens))))) 
  (define (write-to-if aux-result last-else)
    (define (create-if conds)
      (if (null? conds)
          last-else
               #`(if (#,@(car (syntax-e (car conds))))
                  #,@(cdr (syntax-e (car conds)))
                  #,(create-if (cdr conds)))))
    (create-if aux-result))
  (write-aux (syntax-e conds) (syntax-e thens))
  (set! result #`(#,@(write-to-if (reverse aux-result) last-else)))
  (displayln (syntax->datum result)))

(syntax-parse cond-to-if
  [(cond (~seq (e:expr then-stuff) ... [else stuff]))
   (begin 
     #'(e ...)
     (write-if #'(e ...) #'(then-stuff ...) #'stuff))])

