#lang scheme/gui

;;;(provide console-frame)
(provide console:printf)

(require "schemep3-helpers.scm")
(require "schemep3-main-menu.scm")

(define console%  
  (class frame%
    
    (super-new
     (parent #f)
     (width 720)
     (height 300)
     (label "Console"))
        
    (define _canvas 
      (new editor-canvas%
           (parent this)
           (editor (new text%))))
    
    (define _menu-items (list))
    
    (let ([console-menu-item
           (make-main-menu-checkable-item 
            "Console" 
            (lambda (show?)
              (send this show show?))            
            #f
            (lambda (menu-item)
              (push! _menu-items menu-item)))])
      (main-menu:add main-menu:group:view console-menu-item))
    
    (define/augment (on-close)
      (inner (void) on-close)
      (for ((m _menu-items))
        (send m check #f)))
    
    (define insert-semaphore (make-semaphore 1))
    
    ;;;(current-output-port (open-output-text-editor _text-editor))
    
    (define/public (thread-safe-insert . p)
      (call-with-semaphore 
       insert-semaphore
       (lambda () (send (send _canvas get-editor) insert . p))))
    
    (define/public (print string)
      (thread-safe-insert string))))

(define-singleton console-frame console%)

(define (console:printf . p)
  (let ([string (apply format p)])
    (send (console-frame) print string)))
