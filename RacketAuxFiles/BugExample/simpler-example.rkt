#lang racket/base
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define refactoring-tool-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
        (message-box "Plugin Test")
      (let ((btn
             (new switchable-button%
                  (label "Refactoring If")
                  (callback (λ (button)
                              (reverse-content
                               (get-definitions-text))))
                  (parent (get-button-panel))
                  (bitmap reverse-content-bitmap))))
        (register-toolbar-button btn #:number 11)
        (send (get-button-panel) change-children
              (λ (l)
                (cons btn (remq btn l)))))))
  
  (define reverse-content-bitmap
    (let* ((bmp (make-bitmap 16 16))
           (bdc (make-object bitmap-dc% bmp)))
      (send bdc erase)
      (send bdc set-smoothing 'smoothed)
      (send bdc set-pen "black" 1 'transparent)
      (send bdc set-brush "blue" 'solid)
      (send bdc draw-ellipse 2 2 8 8)
      (send bdc set-brush "red" 'solid)
      (send bdc draw-ellipse 6 6 8 8)
      (send bdc set-bitmap #f)
      bmp))
  
  (define (reverse-content text)
    (for ((x (in-range 1 (send text last-position))))
      (send text split-snip x))
    (define snips
      (let loop ((snip (send text find-first-snip)))
        (if snip
            (cons snip (loop (send snip next)))
            '())))
    (define released-snips
      (for/list ((snip (in-list snips))
                 #:when (send snip release-from-owner))
        snip))
    (for ((x (in-list released-snips)))
      (send text insert x 0 0)))
  
  (define (phase1) (void))
  (define (phase2) (void))
  (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))