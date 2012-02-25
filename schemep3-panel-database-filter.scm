#lang scheme/gui

(provide filter-panel%)

(require framework/gui-utils)

(require srfi/2)
(require srfi/26)
 
(require "schemep3-playlist.scm")  
(require "schemep3-database.scm")
(require "schemep3-status.scm")
(require "schemep3-main-menu.scm")
(require "schemep3-mixins-gui.scm")
(require "schemep3-helpers.scm")

(define filter-panel%
  (class (checkable-panel-mixin group-box-panel% "Database Filter")
    
    (define field-labels (list "Artist" "Album" "Title"))
    (define field-text-boxes #f)
    
    (define choice-rating #f)
    (define choice-sort #f)
    (define check-reverse-sort #f)
    
    (define/public (get-selected-rating)
      (let ((r (send choice-rating get-selection)))
        (cond [(= r 0) #f]
              [else (- 6 r)])))
    
    (define (get-query-string)      
      (let ((clauses
             (filter-map
              (lambda (label-string text-field)
                (let ((text (send text-field get-value)))
                  (and (>= (string-length text) 2)
                      (format "~A LIKE \"%~A%\"" label-string text))))
              field-labels
              field-text-boxes)))
        
        (and-let* ((r (get-selected-rating)))
          (push! clauses (format "RATING>=~A" r)))
        
        (and 
         (not (empty? clauses))
         (format "~A ORDER BY lower(~A) ~A" 
                 (string-join clauses " AND ")
                 (send choice-sort get-string-selection)
                 (if (send check-reverse-sort get-value) "DESC" "ASC")))))
    
    (define apply-query
      (let ([cancel-fn #f])        
        (lambda x
          (and-let* ((where-clause (get-query-string)))
            (when cancel-fn
              (cancel-fn))
            (set! cancel-fn
                  (gui-utils:delay-action 
                   1
                   (lambda () 
                     (queue-callback (cut playlist-set-from-database where-clause)))                 
                   (lambda () 
                     (set! cancel-fn #f))))))))
    
    (super-new 
     (label "Database Filter")
     (alignment '(left top))
     (stretchable-height #f))
    
    (let ((h-panel 
           (new horizontal-panel%
                (parent this))))
                  
      (set! field-text-boxes
            (for/list ((label-string field-labels))
              (new text-field%
                   (parent h-panel)
                   (callback apply-query)
                   (label 
                    (format "~A: " label-string)))))
            
      (set! 
       choice-rating
       (new choice%
            (parent h-panel)
            (label "Rating >= ")
            (callback apply-query)
            (choices 
             '("Any" "5" "4" "3" "2" "1"))))
      
      (set! choice-sort
            (new choice%
                 (parent h-panel)
                 (label "Sort: ")
                 (callback apply-query)
                 (choices 
                  '("filename" 
                    "random()"
                    "last_played"
                    "play_count"
                    "rating"
                    "artist" "album" "title"))))
      
      (set! check-reverse-sort
            (new check-box%
                 (parent h-panel)
                 (label "Reverse")
                 (callback apply-query))))))
