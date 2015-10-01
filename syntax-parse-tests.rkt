#lang racket

(define a 1)
(define b 2)
(not (> a b))
(if (= (+ a b) a) 
    #f 
    #t)
(and 1 2)


;(and (< 1 2) (< 2 3))
(if (= 1 2)
    #t
    #f)
#|(provide print-cake)
(provide a-function)

(define (a-function)
  (void))

; draws a cake with n candles
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))
 
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))|#