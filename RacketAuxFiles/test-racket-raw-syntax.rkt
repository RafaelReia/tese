#lang racket
;(#%app call-with-values  (#%app))
((lambda () (if (#%app = (quote 1) (quote 2)) (quote #t) (quote #f))))