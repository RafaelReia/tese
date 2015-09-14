#lang racket

(require syntax/parse)

#;(syntax-parse #'(a b c)
    [(x:id ...) 'ok])
#;(syntax-parse #'(if (< a b) #t #f)
    #:literals (if)
    [(if test-expr then-expr else-expr) #'(if test-expr then-expr else-expr)])

#;(syntax-parse #'(define x 12)
    #:literals (define)
    [(define var:id body:expr) 'ok])
#;(syntax-parse #'(define x 7)
    #:literals (define)
    [(d:define var:id body:expr) #'d])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define c (void))

;;;;;;;;;;;; If to When
(syntax-parse #'(if (< a b) #t (void))
    #:literals (if)
    [(if test-expr then-expr else-expr) 
     (when (equal? '(void) (syntax->datum #'else-expr)) 
       (syntax->datum #'(when test-expr then-expr)))])

;;;;;;;;;;;; If to Unless
(syntax-parse #'(if (< a b) (void) #f)
    #:literals (if)
    [(if test-expr then-expr else-expr)
     (when (equal? '(void) (syntax->datum #'then-expr)) 
       (syntax->datum #'(unless test-expr else-expr)))])


;;If's to cond


;(syntax-parse #'(