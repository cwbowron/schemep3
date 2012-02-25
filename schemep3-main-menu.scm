#lang scheme/gui

(provide
 main-menu:add
 main-menu:find
 main-menu:find-or-create
 main-menu:generate
 
 make-main-menu-separator
 make-main-menu-item
 make-main-menu-group
 make-main-menu-checkable-item
 
 main-menu:group:file
 main-menu:group:view
 main-menu:group:playlist
 main-menu:group:playback
 )

(require srfi/2)

(define main-menu-entity%
  (class object%
    (init-field label)
    (init-field (action #f))
    (define/public (instantiate parent-menu)
      (void))    
    (super-new)))

(define main-menu-item%
  (class main-menu-entity%
    (define/override (instantiate parent)
      (new menu-item%
           (parent parent)
           (label (get-field label this))
           (callback 
            (lambda (menu event)
              ((get-field action this))))))
    (super-new)))

(define main-menu-checkable-item%
  (class main-menu-entity%
    (init-field (checked #f))
    (init-field (init-callback #f))
    (define/override (instantiate parent)
      (let ([menu-item 
             (new checkable-menu-item%
                  (parent parent)
                  (label (get-field label this))
                  (checked (get-field checked this))
                  (callback
                   (lambda (menu event)
                     ((get-field action this) (send menu is-checked?)))))])
        (when init-callback
          (init-callback menu-item))
        menu-item))
    (super-new)))

(define main-menu-separator%
  (class main-menu-entity%
    (define/override (instantiate parent)
      (new separator-menu-item%
           (parent parent)))
    (super-new)))

(define main-menu-group%
  (class main-menu-entity%
    (init-field (children (list)))
    (define/override (instantiate parent)
      (let ((submenu
             (new menu%
                  (parent parent)
                  (label (get-field label this)))))
        (for ((child (get-field children this)))
          (send child instantiate submenu))))
    (define/public (add item)
      (set! children (append (get-field children this)  (list item))))
    (super-new)))

(define (make-main-menu-group label children)
  (new main-menu-group% [label label] [children children]))

(define (make-main-menu-item label action)
  (new main-menu-item% [label label] [action action]))

(define (make-main-menu-separator)
  (new main-menu-separator% [label #f]))

(define (make-main-menu-checkable-item label action checked (init-callback #f))
  (new main-menu-checkable-item% [label label] [action action] [checked checked]
       [init-callback init-callback]))
  
(define main-menu-top-level (make-main-menu-group "top-level" (list)))

(define (main-menu:generate parent)
  (for ((item (get-field children main-menu-top-level)))
    (send item instantiate parent)))

(define (main-menu:add parent group-or-item)  
  (let ([real-parent (or parent main-menu-top-level)])
    (send real-parent add group-or-item)))

(define (main-menu:find label)
  (define (find-in-group group)
    (if (equal? (get-field label group) label)
        group
        (for/or ((child (get-field children group)))
          (and
           (is-a? child main-menu-group%)
           (find-in-group child)))))
  (find-in-group main-menu-top-level))

(define (main-menu:create label)
  (let ([g (make-main-menu-group label (list))])
    (main-menu:add #f g)
    g))

(define (main-menu:find-or-create label)
  (or (main-menu:find label)
      (main-menu:create label)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main-menu:group:file (main-menu:create "&File"))
(define main-menu:group:playback (main-menu:create "&Playback"))
(define main-menu:group:playlist (main-menu:create "Play&list"))
(define main-menu:group:view (main-menu:create "&View"))
