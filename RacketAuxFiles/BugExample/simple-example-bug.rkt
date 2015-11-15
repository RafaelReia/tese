#lang racket
(require drracket/tool-lib
         mred
         framework
         string-constants
         drracket/private/syncheck/traversals
         "eval-helpers-and-pref-init.rkt" ;drracket's file
         )

;;;; NOT WORKING
(define not-expanded-program null)
;;;Hack print-extra-info? 
(define print-extra-info? #f)
(define status-expanding-expression (string-constant cs-status-expanding-expression))
(define syncheck-frame<%>
  (interface ()
    syncheck:button-callback
    syncheck:error-report-visible?
    syncheck:get-error-report-contents))

(displayln "test")
(define unit-frame-mixin ;2842
  (mixin (drracket:unit:frame<%>) (syncheck-frame<%>)
    
    (inherit get-button-panel 
             get-definitions-canvas 
             get-definitions-text
             get-interactions-text
             get-current-tab)
    
    ;; set-syncheck-running-mode : (or/c (box boolean?) 'button #f) -> boolean
    ;; records how a particular check syntax is being played out in the editor right now.
    ;; - #f means nothing is currently running.
    ;; - 'button means someone clicked the check syntax button
    ;;    (or the menu item or keyboard shortcut...)
    ;; - the boxed boolean means that a trace is being replayed from the other place.
    ;;   if the box is set to #f, then the trace replay will be stopped.
    ;; if #f is returned, then the mode change is not allowed; this only happens when
    ;;    a box is passed in
    (define/public (set-syncheck-running-mode mode)
      (cond
        [(not mode)
         (when (box? current-syncheck-running-mode)
           (set-box! current-syncheck-running-mode #f))
         (set! current-syncheck-running-mode #f)
         #t]
        [(box? mode)
         (cond
           [(eq? current-syncheck-running-mode 'button)
            #f]
           [(eq? mode current-syncheck-running-mode)
            ;; this shouldn't happen, I think
            #t]
           [else
            (when (box? current-syncheck-running-mode)
              (set-box! current-syncheck-running-mode #f))
            (set! current-syncheck-running-mode mode)
            #t])]
        [(eq? 'button mode)
         (when (box? current-syncheck-running-mode)
           (set-box! current-syncheck-running-mode #f))
         (set! current-syncheck-running-mode mode)
         #t]
        [else
         (error 'set-syncheck-running-mode "unknown new mode ~s\n" mode)]))
    
    (define current-syncheck-running-mode #f)
    
    (define report-error-parent-panel 'uninitialized-report-error-parent-panel)
    (define report-error-panel 'uninitialized-report-error-panel)
    #;(define report-error-canvas 'uninitialized-report-error-editor-canvas)
    (define/public-final (syncheck:error-report-visible?)
      (and (is-a? report-error-parent-panel area-container<%>)
           (member report-error-panel (send report-error-parent-panel get-children))))
    (define/private (show-error-report)
      (unless (syncheck:error-report-visible?)
        (send report-error-parent-panel stop-recording-prefs)
        (send report-error-parent-panel change-children
              (λ (l) (cons report-error-panel l)))
        (let ([p (preferences:get 'drracket:check-syntax-error-report-window-percentage)])
          (send report-error-parent-panel set-percentages 
                (list p (- 1 p))))
        (send report-error-parent-panel start-recording-prefs)))
    
    (define definitions-text (get-definitions-text))
    (define definitions-text-copy 
              (new (class text:basic%
                     ;; overriding get-port-name like this ensures
                     ;; that the resulting syntax objects are connected
                     ;; to the actual definitions-text, not this copy
                     (define/override (get-port-name)
                       (send definitions-text get-port-name))
                     (super-new))))
    (define settings (send definitions-text get-next-settings))
    (define interactions-text (get-interactions-text))
    (define drs-eventspace (current-eventspace))
    ;; set by the init-proc
    (define expanded-expression void)
    (define expansion-completed void)
    (define user-custodian #f)
    (inherit open-status-line close-status-line update-status-line ensure-rep-hidden)
    (define normal-termination? #f)
    (define the-tab (get-current-tab))
    (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
    (define error-display-semaphore (make-semaphore 0))
    (define uncaught-exception-raised
      (λ () ;; =user=
        (set! normal-termination? #t)
        (parameterize ([current-eventspace drs-eventspace])
          (queue-callback
           (λ () ;;  =drs=
             (yield error-display-semaphore) ;; let error display go first
             (send the-tab syncheck:clear-highlighting)
             (cleanup)
             (custodian-shutdown-all user-custodian))))))
    (define show-error-report/tab
      (λ () ; =drs=
        (send the-tab turn-on-error-report)
        (send (send the-tab get-error-report-text) scroll-to-position 0)
        (when (eq? (get-current-tab) the-tab)
          (show-error-report))))
    (define cleanup
      (λ () ; =drs=
        (send the-tab set-breakables old-break-thread old-custodian)
        (send the-tab enable-evaluation)
        (set-syncheck-running-mode #f) 
        (close-status-line 'drracket:check-syntax:status)
        
        ;; do this with some lag ... not great, but should be okay.
        (let ([err-port (send (send the-tab get-error-report-text) get-err-port)])
          (thread
           (λ ()
             (flush-output err-port)
             (queue-callback
              (λ ()
                (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                  (show-error-report/tab)))))))))
    (define kill-termination
              (λ ()
                (unless normal-termination?
                  (parameterize ([current-eventspace drs-eventspace])
                    (queue-callback
                     (λ ()
                       (send the-tab syncheck:clear-highlighting)
                       (cleanup)
                       (custodian-shutdown-all user-custodian)))))))
    
    (define error-port (send (send the-tab get-error-report-text) get-err-port))
    (define output-port (send (send the-tab get-error-report-text) get-out-port))
    (define init-proc
      (λ () ; =user=
        (send the-tab set-breakables (current-thread) (current-custodian))
        (set-directory definitions-text)
        (current-load-relative-directory #f)
        (current-error-port error-port)
        (current-output-port output-port)
        (error-display-handler 
         (λ (msg exn) ;; =user=
           (parameterize ([current-eventspace drs-eventspace])
             (queue-callback
              (λ () ;; =drs=
                
                ;; this has to come first or else the positioning
                ;; computations in the highlight-errors/exn method
                ;; will be wrong by the size of the error report box
                (show-error-report/tab)
                
                ;; a call like this one also happens in 
                ;; drracket:debug:error-display-handler/stacktrace
                ;; but that call won't happen here, because
                ;; the rep is not in the current-rep parameter
                (send interactions-text highlight-errors/exn exn))))
           
           (drracket:debug:error-display-handler/stacktrace 
            msg 
            exn 
            '()
            #:definitions-text definitions-text)
           
           (semaphore-post error-display-semaphore)))
        
        (error-print-source-location #f) ; need to build code to render error first
        (uncaught-exception-handler
         (let ([oh (uncaught-exception-handler)])
           (λ (exn)
             (uncaught-exception-raised)
             (oh exn))))
        (update-status-line 'drracket:check-syntax:status status-expanding-expression)
        (set!-values (expanded-expression expansion-completed) 
                     (make-traversal (current-namespace)
                                     (current-directory)
                                     print-extra-info?)) ;; set by set-directory above
        (set! user-custodian (current-custodian))))
    (displayln "working")
    ((λ ()
       ;(send the-tab clear-annotations)
       ;(send the-tab reset-offer-kill)
       ;(send the-tab syncheck:clear-highlighting)
       ;(send (send the-tab get-defs) syncheck:init-arrows)
       ((drracket:eval:traverse-program/multiple
         #:gui-modules? #f
         settings
         init-proc
         kill-termination)
        (drracket:language:make-text/pos definitions-text-copy
                                         0
                                         (send definitions-text-copy last-position))
        (λ (sexp loop) ;this is the "iter"
          ;(void) "syntax-parse-tests.rkt:1:0: read: #lang not enabled in the current context" + close-status-line: status line not open 'drracket:check-syntax:status
          (cond
            [(eof-object? sexp)
             (custodian-shutdown-all user-custodian)]
            ;(custodian-shutdown-all user-custodian)]
            [else
             ;(open-status-line 'drracket:check-syntax:status)
             ;(displayln sexp)
             (set! not-expanded-program sexp)
             (loop)])) 
        #t #;(not module-language?))))
    
    ;; set-directory : text -> void
    ;; sets the current-directory based on the file saved in the definitions-text
    (define/private (set-directory definitions-text)
      (define tmp-b (box #f))
      (define fn (send definitions-text get-filename tmp-b))
      (define dir (get-init-dir (and (not (unbox tmp-b)) fn)))
      (current-directory dir))))