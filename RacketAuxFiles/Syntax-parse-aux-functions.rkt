#lang racket

(require syntax/parse)

;;;;; named-let to define (create a function)
;;; (let proc-id ?x ?y)
(syntax-parse #'(let* teste ((a 1 ) (b 2)) (begin (+ a b) (+ b a)))
  #:literals(let*)
  [(let* name ((i e:expr) ...) ?y) #'(define (name i ...) ?y)])

;;;;; let* to defines (use with caution!!)


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

;;;;;;;;; (if (cond) #t (cond)) to OR
(syntax-parse #'(if (< 1 2) #t (< 1 3))
  #:literals(if)
  [(if test-expr #t else-expr)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'else-expr)))
     (syntax->datum #'(or test-expr else-expr)))])



;;;;;;;;; (if (cond) (cond) #f) to AND
(syntax-parse #'(if (< 1 2) (< 1 3) #f)
  #:literals(if)
  [(if test-expr then-expr #f)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'then-expr)))
     (syntax->datum #'(and test-expr then-expr)))])

;;;;;;;; (and (and ?x ... ) ?y...) -> (and ?x ... ?y...)
(syntax-parse #'(and (and (< 1 2) (< 2 3)) (< 3 4) (< 4 5))
  #:literals(and)
  [(and (and ?x ...) ?y ...) #'(and ?x ... ?y ...)])

;;;;;;;; (if ?x ?y #f) -> (when ?x ?y)
(syntax-parse #'(if (< 1 2) (< 2 3) #f)
  #:literals(if)
  [(if ?x ?y #f) #'(when ?x ?y)])

;;;;;;; (when ?x (begin ?y ...)) -> (when ?x ?y ...)
(syntax-parse #'(when (< 1 2) (begin (< 3 2) (< 3 4)))
  #:literals(when begin)
  [(when ?x (begin ?y ...)) #'(when ?x (?y ...))])

;;;;; (let ?x (begin ?y...)) -> (let ?x ?y ...)

(syntax-parse #'(let ((a 1 ) (b 2)) (begin (+ a b) (+ b a)))
  #:literals(let begin)
  [(let ?x (begin ?y ...)) #'(let ?x ?y ...)])
  


;;;;;;;; (and (= stuff 1) (= another-stuff 1))
;;;TODO add ... eq? (and related)
(syntax-parse #'(and (= (+ 1 2) 1) (= (+ 2 1) 1))
  #:literals(and = eq?)
  [(and (= expr val) (= expr2 val2))
   (when (eq? (syntax->datum #'val) (syntax->datum #'val2))
     (syntax->datum #'(and expr expr2 val)))])

