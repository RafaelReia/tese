#lang racket

(define soma +)


;; by applying eta reduction on this
((lambda (x) (car x)) (list 1 2 3))

;; it should get this
;soma

;; should be possible to use eta reduction here.
(map (lambda (x) (abs x))
       list)


;;teste

(lambda (x y) (+ x y))
(lambda (x y) (soma x y))
(lambda (x y) (+ y x))
(lambda (x y z) (+ x y z))
