#lang racket
(require syntax/parse)
(require syntax/to-string)
(define if-to-cond #'(if (< a b)
                         1
                         (if (< b a )
                             2
                             (if (< a c)
                                 5
                                 (if (< c a)
                                     3
                                     (if (< a c)
                                         5
                                         (if (< c a)
                                             (if (< a c)
                                                 5
                                                 (if (< c a)
                                                     3
                                                     5))
                                             (if (< a c)
                                                 5
                                                 (if (< c a)
                                                     3
                                                     (if (< c a)
                                                         3
                                                         (if (< c a)
                                                             5
                                                             (if (< c a)
                                                                 5
                                                                 (if (< c a)
                                                                     (if (< c a)
                                                                         5
                                                                         (if (< c a)
                                                                             5
                                                                             (if (< c a)
                                                                                 (if (< c a)
                                                                                     (if (< c a)
                                                                                         (if (< c a)
                                                                                             5
                                                                                             6)
                                                                                         6)
                                                                                     7)
                                                                                 8)
                                                                             9)))))))))))))))

#;(syntax-parse if-to-cond
    #:literals(if)
    [() 'ok])

(define (parser1 stx)
  (define stx-aux null)
  (define list-tests (list))
  (define list-thens (list))
  (define else-aux null)
  (define (create-conds list-tests)
    (define result null)
    
    (define (create-result lst)
      (unless (null? list-tests)
        (cond [(syntax? list-tests)  (displayln "syntax reached")
                                     (set! result (cons #`(#,list-tests #,list-thens) result))]
              [(pair? list-tests)  (displayln "pair reached")
                                   (set! result (cons #`[#,(car list-tests) #,(car list-thens)] result))
                                   (displayln result)
                                   (set! list-tests (cdr list-tests))
                                   (set! list-thens (cdr list-thens))
                                   (create-result list-tests)]
              [else (displayln "else reached")])))
    (displayln "Creating the result")
    (create-result list-tests)
    ;(displayln result)
    result)  
  
  
  (define (parse-if stx)
    (syntax-parse stx
      [(if test-expr then-expr else-expr)  (begin 
                                             (displayln "weeee")
                                             (set! list-tests (cons #'test-expr list-tests))
                                             (set! list-thens (cons #'then-expr list-thens))
                                             (parse-if #'else-expr))]
      [else (begin
              ;(displayln "end")
              ;(displayln (reverse list-tests))
              ;(displayln (reverse list-thens))
              ;(displayln #'else)
              ;(set! list-tests (reverse list-tests))
              ;(set! list-thens (reverse list-thens))
              (set! else-aux #'else)
              )]))
  ;;#`(+ 1 #,test)
  (parse-if stx)
  #`((cond 
      #,@(create-conds list-tests)
      [else #, else-aux]))
  )

(syntax->string (parser1 if-to-cond))




