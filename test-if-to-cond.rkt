#lang racket
(require syntax/parse)

(define if-to-cond #'(if (< a b)
                         1
                         (if (< b a )
                             2
                             (if (< a c)
                                 5
                                 (if (< c a)
                                     3
                                     4)))))

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
      (begin
        (if (syntax? list-tests)
            (set! result (cons #`([#,list-tests #,list-thens]) result))
            (begin (set! result (cons #`([#,(car list-tests) #,(car list-thens)]) result))
                   (create-result (car list-tests)))))))
    (create-result list-tests))
  (define (parse-if stx)
    (syntax-parse stx
      [(if test-expr then-expr else-expr)  (begin 
                                             (displayln "weeee")
                                             (set! list-tests (cons #'test-expr list-tests))
                                             (set! list-thens (cons #'then-expr list-thens))
                                             (parse-if #'else-expr))]
      [else (begin
              (displayln "end")
              (displayln (reverse list-tests))
              (displayln (reverse list-thens))
              (displayln #'else)
              (set! else-aux #'else)
              )]))
  ;;#`(+ 1 #,test)
  (parse-if stx)
  #`(cond 
      #,(create-conds list-tests)
      [else #, else-aux])
  )

(parser1 if-to-cond)


