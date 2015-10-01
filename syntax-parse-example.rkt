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
#;(syntax-parse #'(if (< a b) #t (void))
    #:literals (if)
    [(if test-expr then-expr else-expr) 
     (when (equal? '(void) (syntax->datum #'else-expr)) 
       (syntax->datum #'(when test-expr then-expr)))])

;;;;;;;;;;;; If to Unless
#;(syntax-parse #'(if (< a b) (void) #f)
    #:literals (if)
    [(if test-expr then-expr else-expr)
     (when (equal? '(void) (syntax->datum #'then-expr)) 
       (syntax->datum #'(unless test-expr else-expr)))])


;;;;;;;;;;;;;;;;;;
;;Make for other types! and add ... for the rest of possibilities
;;(and (< x y) (< y z)) => (< x y z) 
#;(syntax-parse #'(and (< x y) (< y z))
  #:literals(and <)
  [(and (< x y) (< v z))
   (when (equal? (syntax->datum #'y) (syntax->datum #'v))
     (syntax->datum #'(< x y z)))])

;;;;;;;;;;;;;;;;;;;;;
;;add ... for the rest of possibilities
;;(not (< x y)) => (>= x y)
#;(syntax-parse #'(not (< x y))
  #:literals(not <)
  [(not (< x y))
   (syntax->datum #'(not (>= x y)))])

;;(not (>= x y)) => (< x y)
#;(syntax-parse #'(not (>= x y))
  #:literals(not >=)
  [(not (>= x y))
   (syntax->datum #'(not (< x y)))])

;;(not (> x y)) => (<= x y)
#;(syntax-parse #'(not (> x y))
  #:literals(not >)
  [(not (> x y))
   (syntax->datum #'(<= x y))])

;;(not (<= x y)) => (> x y)
#;(syntax-parse #'(not (<= x y))
  #:literals(not <=)
  [(not (<= x y))
   (syntax->datum #'(not (> x y)))])

;;;;;;
(syntax-parse #'(if (= (mod n 2) 1) #f #t)
  #:literals(if)
  [(if test-expr then-expr else-expr)
   (when (equal? (syntax->datum #'then-expr) (not (syntax->datum #'else-expr)))
     (syntax->datum #'(not test-expr)))])

;;;;;;;;;;;;;;;;;;;;;;;;
; weird test
;(define test (datum->syntax #f (not (<= 1 2))))

#;(syntax-parse test
  #:literals(not <=)
  [(not (<= x y))
   (syntax->datum #'(not (> x y)))])
;(#%app call-with-values lambda if #%app = #%app + quote 1 quote 2 quote 1 quote #f quote #t print-values)


#;(syntax-parse #'(if (#%app = #%app + quote 1 quote 2) quote 1 quote #f quote #t)
  #:literals(#%app call-with-values lambda if = quote)
  [(#%app call-with-values lambda if test-expr quote val3 quote val4 quote val5 print-values) 'ok])


;syntax-parse-example.rkt:86:2: syntax-parse: expected non-empty clause body at:
;((#%app call-with-v...l-with-values lambda if = quote print-values) ((#%app call-with-values lambda if #%app = #%app ...

(define arg #'(if (= (+ a b) a) #f #t))
(syntax-parse arg
                  #:literals(if)
                  [(if test-expr then-expr else-expr) (syntax->datum #'(not test-expr))])