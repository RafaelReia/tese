#lang racket
(require syntax/parse)
(define cond-to-if #'(cond ([(< 1 2) #t]
                            [(< 2 3) #t]
                            [(< 1 2) #t]
                            [else #t])))
(define (write-if conds thens last-else)
  (define result null)
  (define (write-aux conds thens)
    (unless (null? conds)
      (begin
        (displayln (syntax? conds))
        (set! result (cons #`(#,(car conds) #,(car thens)) result))
        (write-aux (cdr conds) (cdr thens)))))
  (write-aux conds thens)
  (displayln result))

(syntax-parse cond-to-if
  [(cond ((~seq (e:expr then-stuff) ...) [else stuff]))
   (begin 
     #'(e ...)
     #;(write-if #'(e ...) #'(then-stuff ...) #'stuff))])

