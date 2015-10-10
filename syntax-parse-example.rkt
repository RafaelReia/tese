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

(define if-to-cond #'(if (< a b)
                         1
                         (if (< b a )
                             2
                             (if (< a c)
                                 5
                                 (if (< c a)
                                     3
                                     4)))))
(define a 1)
(define b 1)
(define c 1)
(cond [(< a b) 1]
      [(< b a) 2]
      [(< c a) 3]
      [else 4])
(define parser1
  (syntax-parser
    [((~or (~once (~seq #:a x) #:name "#:a keyword")
           (~optional (~seq #:b y) #:name "#:b keyword")
           (~seq #:c z)) ...)
     'ok]))

(syntax-parse if-to-cond
  #:literals( if)
  [((~or (~once (~seq if test-expr then-expr else-expr) #:name "If-last-part" ) ;exactly once
         ;(~optional (~seq if test-expr then-expr else-expr)) ; maximum one
         (~optional (~seq test-expr4 then-expr4))
         (~optional (~seq if test-expr5 then-expr5))
         (~seq test-expr3 then-expr3 if)
         (~seq if test-expr2 then-expr2 if)) ...)
   ;at will
   #'else-expr])


#;#'(if test-expr2 ... then-expr2 ... (if test-expr then-expr else-expr))
#;#'(cond [test-expr2 ... then-expr2 ...] 
          [test-expr then-expr]
          [else else-expr])



#;(syntax-parse if-to-cond
    #:literals(if)
    [(~or (if test-expr then-expr else-expr) (if test-expr then-expr (if test-expr1 then-expr1))) #'test-expr ])
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; if's to cond

#;(syntax-parse #'(if (< a b) (if (< a b) #t #f) #f)
    #:literals (if)
    [(if test-expr (if test-expr then-expr else-expr) else-expr)])

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
#;(syntax-parse arg
    #:literals(if)
    [(if test-expr then-expr else-expr) (syntax->datum #'(not test-expr))])
(define-syntax-class (nat-less-than n)
  (pattern x:nat #:when (< (syntax-e #'x) n)))

(syntax-parse #'(1 2 3 4 5)
  [((~var small (nat-less-than 4)) ... large:nat ...)
   (list #'(small ...) #'(large ...))])

(syntax-parse #'(= (lenght l) 0)
  [(= (lenght l) 0) (syntax->datum #'(null? l))])

(syntax-parse #'(= (lenght l) 1)
  [(= (lenght l) 1) (syntax->datum #'(singleton? l))])

(syntax-parse #'(cons 1 (list 2 3 4 5 6))
  #:literals(list)
  [(cons x (list y ... v))
   (syntax->datum #'(list x y ... v))])


(map (lambda (l) (car l)) '((1 2)(3 4)))
(map car '((1 2)(3 4)))

(syntax-parse #'(map (lambda (l) (car l)) '((1 2)(3 4)))
  #:literals(map lambda)
  #:datum-literals(l)
  [(map (lambda (l) (function l)) arg) #'(map function arg)])

