#lang racket

(require syntax/parse)

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

;;;;;;;;; (if (cond) #t (cond))
(syntax-parse #'(if (< 1 2) #t (< 1 3))
  #:literals(if)
  [(if test-expr #t else-expr)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'else-expr)))
     (syntax->datum #'(or test-expr else-expr)))])



;;;;;;;;; (if (cond) (cond) #f)
(syntax-parse #'(if (< 1 2) (< 1 3) #f)
  #:literals(if)
  [(if test-expr then-expr #f)
   (when (and (boolean? (eval-syntax #'test-expr)) (boolean? (eval-syntax #'then-expr)))
     (syntax->datum #'(and test-expr then-expr)))])



;;;;;;;; (and (= stuff 1) (= another-stuff 1))
;;;TODO add ... eq? (and related)
(syntax-parse #'(and (= (+ 1 2) 1) (= (+ 2 1) 1))
  #:literals(and = eq?)
  [(and (= expr val) (= expr2 val2))
   (when (eq? (syntax->datum #'val) (syntax->datum #'val2))
     (syntax->datum #'(and expr expr2 val)))])

