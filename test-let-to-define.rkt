#lang racket
(require syntax/parse)
(let ((a 1)
      (b 1))
  (+ a b))
(define to-parse #'(let ((a 1)(b 1))(+ a b)))

(syntax-parse #'(#:a 1 #:b 2 3 4 5)
  [((~and (~seq (~seq k:keyword e:expr) ...)
          (~seq keyword-stuff ...))
    positional-stuff ...)
   (syntax->datum #'((k ...) (e ...) (keyword-stuff ...)))])
#;(syntax-parse to-parse
    [((~and (let (~seq (~seq i:id e:expr) ...)
              (~seq else ...))))
     #'((i ...) (e ...) (else ...))])

;

#;(syntax-parse #'(let ((a 1)(b 1))(+ 1 2))
    #:literals(let)
    [(let (~and (~seq (~seq i e:expr) ...)
                else:expr))
     #'((i ...) (e ...) else)])


(define ids null)
(define exprs null)
(define (create-define)
  (define result null)
  (define (create-result)
    (unless (null? ids)
      (cond [(pair? ids) (set! result (cons #`[#,(car ids) #,(car exprs)] result))
                            (displayln result)
                            (set! ids (cdr ids))
                            (set! exprs (cdr exprs))
                            (create-result)]
            [(syntax? ids) (set! result (cons #`(#,ids #,exprs) result))]
            [else (displayln "else reached")])))
  
  
  (create-result)
  (displayln "result")
  (displayln result)
  result)


(syntax-parse #'(let ((a 1) (b 2)) (+ 1 2))
  [(let ((~seq (~seq (i e:expr)) ...)) else ) 
   (begin 
     (set! ids (syntax-e #'(i ...)))
     (set! exprs (syntax-e #'(e ...)))
     (displayln ids)
     (displayln exprs)
     #;(create-define))])
