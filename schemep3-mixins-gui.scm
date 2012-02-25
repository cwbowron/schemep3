#lang scheme/gui

(provide checkable-panel-mixin)
(provide stored-value-mixin)

(require framework)

(require "schemep3-main-menu.scm")

(define (checkable-panel-mixin % menu-string)
  (class % (super-new)    
    (let ([view-item (make-main-menu-checkable-item 
                      menu-string 
                      (lambda (checked?)
                        (if checked?
                            (send (send this get-parent) add-child this)
                            (send (send this get-parent) delete-child this)))
                      #t)])
      (main-menu:add main-menu:group:view view-item))))

(define (stored-value-mixin %)
  (class %
    (init-field settor-gettor)

    (super-new)
    
    (send this set-value (settor-gettor))
    
    (preferences:add-on-close-dialog-callback 
     (lambda ()
       (settor-gettor (send this get-value))
       (void)))))
