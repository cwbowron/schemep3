#lang scheme/gui

(provide ratings-panel%)

(require "schemep3-playlist.scm")
(require "schemep3-database.scm")
(require "schemep3-context-menu.scm")
(require "schemep3-mixins-gui.scm")

(define cooler-button%
  (class canvas%
    
    (init-field bitmap)
    
    (super-new
     (style '())
     (min-width (+ 2 (send bitmap get-width)))
     (min-height (+ 2 (send bitmap get-height)))
     (stretchable-width #f)
     (stretchable-height #f)
     (paint-callback
      (lambda (button dc)
        (send dc draw-bitmap bitmap 0 0))))))

(define ratings-panel%
  (class (checkable-panel-mixin horizontal-panel% "Ratings Buttons")
    (super-new 
     (stretchable-height #f))

    (for ((n (in-range 1 6)))
      (new button%
           (label 
            (make-object bitmap%
              (format "etc/~A.gif" n)))
           (callback 
            (lambda (b e) 
              (rate-items n (playlist-selected-database-indexes))))
           (min-width 24)
           (min-height 24)
           (stretchable-height #f)
           (stretchable-width #f)
           (parent this)))
          
    
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap 
    ;            (make-object bitmap% "etc/1.gif"))) 
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap 
    ;            (make-object bitmap% "etc/2.gif")))
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap 
    ;            (make-object bitmap% "etc/3.gif")))
    
    )
  )

