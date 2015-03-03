#lang racket

(define x (list 1 2 3))

(car x)
;-> eta abstraction
((lambda (x) (car x)) x)
;-> eta abstraction
((lambda (x) ((lambda (x) (car x)) x)) x)


(quote 1)

