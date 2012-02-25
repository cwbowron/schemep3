#lang scheme/gui
(provide playback-control-panel%)

;;; plt modules

;;; local modules
(require "schemep3-playback.scm")

;  (define cooler-button%
;    (class canvas%
;      (init-field bitmap)
;      (init-field (mask #f))
;      (super-new
;       (style '(transparent border))
;       (min-width (+ 4 (send bitmap get-width)))
;       (min-height (+ 4 (send bitmap get-height)))
;       (stretchable-width #f)
;       (stretchable-height #f)
;       (paint-callback
;        (lambda (button dc)
;          (send dc draw-bitmap bitmap 1 1 'solid (send the-color-database find-color "black") mask)
;          )))))
(define _bitmap-button-label #t)

(define playback-control-panel%
  (class horizontal-panel%
    (super-new
     (alignment '(right top))
     (stretchable-height #f))
    
    (for ((button-label 
           (if _bitmap-button-label 
               (list
                (make-object bitmap% "etc/stop.png")
                (make-object bitmap% "etc/pause.png") 
                (make-object bitmap% "etc/play.png") 
                (make-object bitmap% "etc/next.png"))
               (list "Stop" "Pause" "Play" "Next")))
          (button-function
           (list stop toggle-pause play
                 next)))
      (new button%
           (label button-label)
           (parent this)
           (callback 
            (lambda (b e) 
              (button-function)))))))
    
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap (make-object bitmap% "etc/stop.png" 'png))
    ;           (mask (make-object bitmap% "etc/stop.png" 'png/mask)))
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap (make-object bitmap% "etc/pause.png"))
    ;           (mask (make-object bitmap% "etc/pause.png" 'png/mask)))
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap (make-object bitmap% "etc/play.png"))
    ;           (mask (make-object bitmap% "etc/play.png" 'png/mask)))
    ;      (new cooler-button%
    ;           (parent this)
    ;           (bitmap (make-object bitmap% "etc/next.png"))
    ;           (mask (make-object bitmap% "etc/next.png" 'png/mask)))