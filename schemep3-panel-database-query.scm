#lang scheme/gui

(provide query-panel%)

(require framework)

(require "schemep3-playlist.scm")  
(require "schemep3-status.scm")
(require "schemep3-mixins-gui.scm")
(require "schemep3-helpers.scm")

(define query-panel%
  (class (checkable-panel-mixin group-box-panel% "Database Query")
    
    (define default-query-choices 
      (list
       "RATING=5 ORDER BY RANDOM()"
       "RATING>3 AND last_played IS NULL ORDER BY RANDOM()"
       "RATING>3 AND artist NOT LIKE \"%beatles%\" ORDER BY RANDOM()"
       "RATING=5 AND ARTIST LIKE \"%Nirvana%\""
       "filename NOT NULL"
       ))

    (define query-choices (preferences-backed-variable 'schemep3-queries default-query-choices))

    (define (add-choice-if-necessary addition)
      (let ((choices (query-choices)))
        (unless (member addition choices)
          (let ((z (append choices (list addition))))
            (query-choices z)
            (send query-text-field append addition)))))
    
    (define (apply-query . ignore)
      (let ((query-string (send query-text-field get-value)))
        (playlist-set-from-database query-string)
        (add-choice-if-necessary query-string)))
    
    (super-new 
     (label "Database query")
     (alignment '(left top))
     (stretchable-height #f))
    
    (define query-sub-panel 
          (new horizontal-panel%
               (parent this)))
    
    (define query-text-field
          (new combo-field%
               (parent query-sub-panel)
               (label #f)
               (choices (query-choices))
               (callback 
                (lambda (tf e)
                  (when (equal? 'text-field-enter (send e get-event-type))
                    (apply-query))))))
    
    (define query-apply-button 
          (new button%
               (label "Apply")
               (parent query-sub-panel)
               (callback apply-query)))))
