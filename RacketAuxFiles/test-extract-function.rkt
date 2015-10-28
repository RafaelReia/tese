#lang racket



;easy test
(define (fun a b)
  (+ a b)
  )


;test let
(define (teste a b)
 (let ([a 7]
        [d 6])
    (+ a b d)))


;test dup args
(define (aux a b c d e)
  (if (< a b)
      (+ a c)
      (- d b))
  )



(let* ([a 7]
       [d (lambda () a)])
  (d))

((lambda (x) (car x) ) (list 1 2 3))
; eta reduction


((lambda () (values + 2 3))) ;rebenta
((lambda () (values 1 2 3))) ;rebenta extraindo o lambda completo ou os values todo




(define f 5)

(define (usando a b)
  (+ a b f))

(let ([g 6])
  (let ([h 8])
    (+ g h)))


(define x (make-parameter 1))
(x)
(parameterize  ((x 10))
 (x))


(let fac ([n 10])
 (if (zero? n)
     1
     (fac (sub1 n))))
