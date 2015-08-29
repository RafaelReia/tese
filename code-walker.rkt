#lang racket
(require racket/list)
(provide code-walker)
;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;
(define syntax-list (list)) ;global, oh well 



;;;;;;;;;; END Definitions ;;;;;;;;;;;;;;;;

;;;;;;;;;; Prepare ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Clean up AST
(define (walk-trought program current next previous)
  (if (null? next)
      (list)
      (begin
        (display "Current: ")
        (displayln current)
        (display "PAIR? ")
        (displayln (pair? next))
        ;(displayln (cdr program-aux))
        (display "Syntax? ")
        (displayln (syntax? next))
        (cond
          [(syntax? next) ;Found syntax, go to there.
           (begin
             (displayln " SYNTAX ")
             (display "DATUM ")
             (displayln (syntax->datum current))
             ;(go-to-syntax (syntax-e current))
             (define aux (syntax-e current))
             (displayln (syntax->datum current))
             (syntax-walker aux (car aux) (cdr aux) null )
             )]
          [else 
           (begin
             (displayln "NEXT PAIR")
             (cons current
                   (walk-trought (cdr program)
                                 (cadr program)
                                 (cddr program) 
                                 null)))]))))
(define (go-to-syntax program-structure)
  (display "[go-to-syntax] program-structure null? ")
  (displayln (null? program-structure))
  (unless (null? program-structure)
    (walk-trought program-structure (car program-structure) (cdr program-structure) null ))
  (displayln "!!!!!!!!!!!! END OF FILE !!!!!!!!!!!!")
  )

(define (syntax-walker program current next previous)
  ;previous is not working.
  (define iteration 0)
  (define syntax-ret null)
  (displayln "Syntax-walker in the HOUSE!!")
  (let loop ((program program)
             (current current)
             (next next)
             (previous previous))
    (display "[Syntax-walker]  Current: ")
    (displayln current)
    (display "[Syntax-walker]  PAIR? ")
    (displayln (pair? next))
    (display "[Syntax-walker]  Syntax Next? ")
    (displayln (syntax? next))
    ;Need a test to know if it is a syntax node of the program.
    (display "[Syntax-walker]  Syntax Current ")
    (displayln (syntax? current))
    
    #|If #t must go and visit each syntax node of the program
      Have a way if it is not true to go back in the code.|#
    
    (cond
      [(pair? next) 
       (if (pair? (cdr program))
           (begin ;hack!
             (when (eq? iteration 1)
               (begin
                 (displayln "SET MADE!")
                 (set! syntax-ret program)))
             ;(set! syntax-list (cons syntax-list current))
             (set! iteration (add1 iteration))
             (loop (cdr program) (cadr program) (cddr program) current)
             )
           (begin
             (displayln "!!!!!!!!!!!!!!!! This should not happen !!!!!!!!!!!!!!!!")
             (display "!!!!!!!!!!!!!!!!")
             (displayln (car program)); do stuff
             (display "!!!!!!!!!!!!!!!!")
             (displayln (syntax? (car program)))
             )
           )]
      [(and (syntax? next) (not (null? (syntax-e next)))) (loop (syntax-e next) (car (syntax-e next)) (cdr (syntax-e next)) current)]
      [(not (or (pair? next) (syntax? next))) (displayln "$$$$$$$$$$$$$$$ End File $$$$$$$$$$$$$$$")]
      [else (displayln "!!!!!!!!!!!!!!!! Case not Supported !!!!!!!!!!!!!!!!")])) ;this happens on a black file. Find out why.
  (set! syntax-list syntax-ret)
  (explore-nodes syntax-ret) ;TODO change this.
  #;(displayln (syntax->datum program)))

#|explore-nodes receives a syntax-list of the first level of the program, avoiding the need to explore every single node of the program.
  
 |#
(define (explore-nodes syntax-list)
   
  #|(define (next-node)
    ;check this out
    (displayln "next node")
    (set! level (sub1 level))
    (set! offset (add1 offset)))
  (define (previous-node)
    ;figure out what to do with this.
    (displayln "previous node"))|#
  (displayln "EXPLORING LIST")
  ;(check-next-offset)
  (displayln syntax-list-aux)
  (display "[TEST] Car: ")
  (displayln (car syntax-list-aux))
)

;;;;;;;;;; Definitions of Search ;;;;;;;;;;;;;
(define syntax-list-aux syntax-list)
  (define stack (list))
  (define level 0)
  (define offset 0)
  (define previous-node null)
  (define node null)
  (define next-node null)
  (define (reset-offset)
    (set! offset 0))
  (define (check-next-offset)
    (define aux (+ offset 1))
    (define aux-list syntax-list-aux) ;correct level
    
    (define (check-offset value)
      (if (= value 0)
          #t
          (if (pair? aux-list)
              (begin
                (set! aux-list (cdr aux-list))
                (check-offset (sub1 value))
                )
              #f)))
    #;(displayln (check-offset 2))
    (if (null? aux-list)
        #f
        (check-offset aux))
    )
  #|Level increases when entering a syntax object (syntax-e)
    If previous-node is null and level is > 0 you go down a level to the corresponding offset
    If next-node is null It may assume that you finish the syntax object, you can go
 to the next syntax object, by going up a level and increasing the offset. If that is not possible it ends the search.
    Selecting a syntax object increases the level.
    Going to the next syntax object increases the offset.
   |#
  (define (go-to-place level offset syntax-list)
    ;this function does not have tests. all checks must be done before.
    (display "[GO-TO-PLACE] level: ")
    (displayln level)
    (display "[GO-TO-PLACE] offset: ")
    (displayln offset)
    (if (= 0 level offset)
        syntax-list
        (cond
          [(and 
            (not (pair? syntax-list)) (syntax? syntax-list)) 
           (begin
             (display "[GO-TO-PLACE] ")
             (displayln syntax-list)
             (go-to-place level offset (syntax-e syntax-list)))]
          [(> level 0) 
           (begin 
             (displayln "[GO-TO-PLACE] Level entered")
             (go-to-place (sub1 level) offset (car syntax-list)))]
          [(> offset 0) (go-to-place level (sub1 offset) (cdr syntax-list))]
          [else 
           (begin
             (displayln "!!!!!!!!!!!!!!!! Case not Supported !!!!!!!!!!!!!!!!")
             (syntax-list))]))) 
  (define (select-syntax-object) ;down a level
    ;do checks
    (if  (null? syntax-list-aux) 
         null
         (if (syntax? (car syntax-list-aux)) ;selects the syntax-object to further inspection
             (begin
               (displayln "Selecting syntax")
               (set! stack (cons (car syntax-list-aux) stack ))
               (set! syntax-list-aux (syntax-e (car syntax-list-aux))) ;should it be done in go-to-place?
               (set! level (add1 level))
               (reset-offset)
               (go-to-place level offset syntax-list))
             (begin
               (displayln "[exit-syntax-object] error: It's not a syntax")
               null))))
  (define (exit-syntax-object) ;up a level
    (if (> level 0)
        (begin
          (displayln "going up to Parent")
          (set! level (sub1 level))
          (reset-offset) ;reset offset Maybe create function
          (set! stack (first stack))
          (set! syntax-list-aux stack)
          (go-to-place level offset syntax-list))
        (begin
          (displayln "[exit-syntax-object] error: level is already 0")
          null)))
  
  (define (next-syntax-object)
    (if (check-next-offset) 
        ;it only checks the next offset, should it go up one level? 
        (begin
          (displayln "Next Syntax")
          (set! offset (add1 offset))
          (go-to-place level offset syntax-list))
        (begin
          ;(displayln "[exit-syntax-object] error: There is no next syntax (can not increase offset)")
          (if (> level 0)
              (begin 
                (displayln "[Up-a-Level] can not increase offset") 
                (exit-syntax-object)
                (next-syntax-object))
              (displayln "[exit-syntax-object] error: can not increase offset (end of the program) "))
          null)))
  (define (previous-syntax-object)
    ;or sub1 in the level and return void/error
    (if (> offset 0)
        (begin
          (displayln "Previous syntax")
          (set! offset (sub1 offset))
          (go-to-place level offset syntax-list))
        (begin
          (if (> level 0)
              (begin 
                (displayln "[Up-a-Level] can not increase offset") 
                (exit-syntax-object)
                (next-syntax-object))
              (displayln "[exit-syntax-object] error: can not decrease offset (start of the program)"))
          null)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; Search Syntax ;;;;;;;;;;;;;;;;
(define (compare-syntax current syntax-wanted)
  ;Receives 2 syntax objects. create contract? 
  (display "Checking syntax: Current ")
  (display current)
  (display "  Wanted ")
  (displayln syntax-wanted)
  (displayln (free-identifier=? current syntax-wanted))
  (free-identifier=? current syntax-wanted)
  )
(define (find-syntax-object source syntax-wanted)
  (define (get-next-syntax-object source)   
    (void))
  (let loop ((source source)
             (syntax-wanted syntax-wanted)
             (syntax-tested (get-next-syntax-object source)))
    ;update source!!
    (if (compare-syntax syntax-tested syntax-wanted)
        #t
        (loop source syntax-wanted (get-next-syntax-object source))))
  )
;;;;;;;;;; Search Patterns ;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





  #|(display "[TEST] car + syntax-e: ")
  (displayln (syntax-e (car syntax-list-aux)))
  (display "[TEST] (...) cdr (car + syntax-e): ")|#
  ;midle steps!
  #|
  (display "[TEST-Middle] Syntax-e car Syntax-List")
  (displayln (syntax-e (car (syntax-list-aux))))
  (display "[TEST-Middle] Syntax-e car cdr + previous: ")
  (displayln (syntax-e (car (cdr (syntax-e (car (syntax-list-aux)))))))
  (display "[TEST-Middle] Syntax-e cdr cdr + previous: ")
  (displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux))))))))))
  (display "[TEST-Middle] Syntax-e car + previous: ")
  (displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux))))))))))))
  |#
  ;test if
  #|
  (displayln (car (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux)))))))))))))
  (display "[TEST] FREE-IDENTIFIER=?: ")
  (displayln (free-identifier=? (car (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux)))))))))))) 
                                #'if))
|#
#|  (display "[TEST] pair? car+syntax-e: ")
  (displayln (pair? (syntax-e (car syntax-list-aux)))) 
  (select-syntax-object)
  (displayln syntax-list-aux)
  (exit-syntax-object)
  (displayln syntax-list-aux)
  ;maybe going down to much, mixin between car and syntax-e. must check this.
  ;(displayln (go-to-place 0 2 syntax-list))
  )|#

(define (code-walker code)
  #;(define program-aux program-structure)
  (define final-list null)
  (go-to-syntax (syntax-e code))
  (displayln "END FILE"))


#|(displayln  program-structure)
  #;(for ((sexp (in-list  program-structure)))
      (displayln sexp))
  #;(displayln (syntax? code))
  #;(displayln (identifier? code))
  #;(displayln (syntax-source code))
  #;(displayln (syntax-line code))
  #;(displayln (syntax-column code))
  
  #;(displayln code) ;in theory this is the syntax object of the program
  ;test One layer
  #;(displayln (syntax-e code))
  ;test EVERYTHING
  #;(displayln (syntax->datum code)) |#

