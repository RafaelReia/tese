#lang racket
(define (fibs n)
  (let ((fibs
         (let loop ((previous 0)
                    (current 1)
                    (index 0))
           (if (= index n)
               (list)
               (cons current
                     (loop current
                           (+ previous current)
                           (+ index 1)))))))
    
    (for ((fib (in-list fibs)))
      (displayln fib))))


(define (compute-fibonacci n)
  (let loop ((previous 0)
             (current 1)
             (index 0))
    (if (= index n)
        (list)
        (cons current
              (loop current
                    (+ previous current)
                    (+ index 1))))))
(define (print-list fibs)
  (for ((fib (in-list fibs)))
    (displayln fib)))
(define (fibsR n)
  (let ((fibs
         (compute-fibonacci n)))
    (print-list fibs)))




#;#;(define (fib n)
      (define fib_numbers (list))
      (let ((previous 0)
            (current 1)
            (index 0))
        (for ([index (in-range n)])
          ;(displayln index)
          (set! fib_numbers (append fib_numbers (list (+ current previous))))
          (set! previous current)
          (set! current (+ current previous))
          )
        (for ([fib (in-list fib_numbers)])
          (displayln fib)
          )
        ))

(define (append-element lst elem)
  (foldr cons (list elem) lst))

;cons + reverse
;usar vector
;falar que as refactoringtools inserem erros no codigo, falar do rename do DrRacket
;e que vamos tentar proteger o utilizador disso
;, e' "impossivel" ser bug free mas vamos ter atencao a isso
