#lang racket

(require syntax/parse)

(syntax-parse #'(a b c)
    [(x:id ...) 'ok])
(syntax-parse #'(if (< a b) #t #f)
  [(