#lang scheme/gui

(provide 
 progress-view%
 playback-time-panel%
 )

(require srfi/2)
(require srfi/26)
(require "schemep3-playback.scm")
(require "schemep3-helpers.scm")
(require "schemep3-status.scm")
(require "schemep3-database.scm")

(define playback-time-panel%
  (class horizontal-panel%
    (super-new
     (stretchable-width #f))
    
    (define time-display-message%
      (class message%
        (define/public (set-time n)
          (send this set-label (format-time (number->integer n))))
        (super-new (label "xx:xx"))))
    
    (define _progress-message 
      (new time-display-message%
           (parent this)))
    
    (new message% (label "/") (parent this))

    (define _duration-message
      (new time-display-message%
           (parent this)))

    (define (set-duration-from-item file-index)
      (and-let* ((duration (schemep3-database-retrieve-field file-index 'duration)))
         (send _duration-message set-time duration)))

    (and-let* ((file-index (now-playing-database-index)))
      (set-duration-from-item file-index))
    
    (add-progress-hook
     (lambda (time position)
       (send _progress-message set-time time)))
    
    (add-pre-play-hook
     (lambda (playlist-index file-index)
       (set-duration-from-item file-index)))))

(define progress-view%
  (class horizontal-panel%    
    (super-new)            
    
    (define _playback-slider
      (new slider%
           (parent this)
           (min-value 0)
           (max-value 100)
           (style '(horizontal plain))
           (callback 
            (lambda (control event)
              (seek (send control get-value))))
           (label #f)))

    (new playback-time-panel%
         (parent this))
    
    (add-progress-hook 
     (lambda (time position)
       (send _playback-slider set-value (number->integer position))))
    ))
