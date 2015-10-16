#lang racket
(require syntax/parse)
(define cond-to-if #'(cond ([(< 1 2) #t]
                            [(< 2 3) #t]
                            [(< 1 2) #t]
                            [else #t])))

(syntax-parse cond-to-if
  [(cond ((~seq (e:expr then-stuff) ...) [else stuff])) #'(then-stuff ...)])
