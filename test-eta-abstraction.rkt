#lang racket

(define x (list 1 2 3))

(car x)
;-> eta abstraction
((lambda (x) (car x)) x)
;-> eta abstraction
((lambda (x) ((lambda (x) (car x)) x)) x)


(quote 1)

(define soma +)

(+ 1 2)


(soma 1 2)
((lambda (x y) (soma x y)) 1 2)

