#lang racket

(define soma +)


;; by applying eta reduction on this
(lambda (x) (car x)) 

;; it should get this
;soma

;; should be possible to use eta reduction here.
(map (lambda (x) (abs x))
       list)


;;teste

(lambda (x y) (+ x y))
