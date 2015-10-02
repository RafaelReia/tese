#lang racket
(define a 1)
(define b 2)
;;;Not
(not (> a b))
(not (>= a b))
(not (< a b))
(not (<= a b))
;; IF
(if (= (+ a b) a) 
    #f 
    #t)
(if (= (+ a b) a) 
    #t 
    #f)
;;; And
(and (< 1 2) (< 2 3))

(displayln "end")










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