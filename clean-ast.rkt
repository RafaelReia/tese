#lang racket


(define (CleanAst )
  (define aux  "(not (#%app = (#%app + a b) a))")
(define transporter-string "")
(define result  aux)
(define count 0)
(set! count (length(regexp-match #rx"(#%app)" aux)))
(define (clean-str)
  (when (regexp-match #rx".*#%app" aux) ;gives true or false.
    (begin
      (set! transporter-string (car (regexp-match #rx".*#%app" aux)))
      (set! result (string-append (substring result 0 (- (string-length transporter-string) (string-length "#%app")))
                                  (substring result (+ (string-length transporter-string) 1))))
      (set! aux (substring aux 0 (- (string-length transporter-string) (string-length "#%app"))))
      (set! transporter-string "")
      (clean-str))))
(clean-str)
;(displayln result)
result)

(CleanAst)
;(displayln aux)

;(set! count (length(regexp-match #rx"(#%app)" "(not (#%app = (#%app + a b) a))")))
;(displayln count)
;(regexp-match #rx".*#%app" "(not (#%app = (#%app + a b) a))")

;(regexp-match #rx".*#%app" "(not (#%app = (+ a b) a))")
;(regexp-match #rx".*#%app" "(not (")