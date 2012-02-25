#lang scheme/gui

(provide
 playlist-search-frame
 show-playlist-search
 )

(require srfi/2)

(require "schemep3-playlist.scm")
(require "schemep3-helpers.scm")
(require "schemep3-database.scm")
(require "schemep3-status.scm")

(define playlist-search-text-field%
  (class text-field%
    
    (define last-list (list))
    (define last-index 0)
    
    (define (field-match? rx item field)
      (and-let* ((value (schemep3-database-retrieve-field item field)))
        (regexp-match rx value)))
    
    (define (item-matches? item text)
      (let ([rx (regexp-quote text #f)])
        (or 
         (field-match? rx item 'artist)
         (field-match? rx item 'album)
         (field-match? rx item 'title))))
    
    (define (find-match text (items #f))
      (let loop ([text text]
                 [n (if items 0 (add1 last-index))]
                 [items (or items (cdr last-list))])
        (cond
          [(null? items)
           (status:update "End of List")]
          [(item-matches? (car items) text)
           (set! last-index n)
           (set! last-list items)           
           (playlist-select last-index)           
           (let ([filename (schemep3-database-index->filename (car items))])
             (status:update "Match: ~A ~A" last-index filename))]
          [else
           (loop text (add1 n) (cdr items))])))
    
    (super-new 
     [label "Find: "]
     [min-width 100]
     [init-value ""]
     [callback
      (lambda (control event)
        (case (send event get-event-type)
          [(text-field-enter)
           (find-match (send control get-value))]
          [(text-field)
           (find-match (send control get-value) (playlist-contents))]))])))

(define playlist-search-frame%
  (class frame%
    
    (define/override (on-subwindow-char control event)
      (or (super on-subwindow-char control event)
          (case (send event get-key-code)
            [(escape)
             (send this show #f)
             #t]
            [else #f])))
    
    (define/override (on-focus on?)
      (when on?
        (send search-text-field focus)))
    
    (super-new
     (label "Find..."))
    
    (define search-text-field
      (new playlist-search-text-field%
           [parent this]))))

(define-lazy-singleton playlist-search-frame playlist-search-frame%)

(define (show-playlist-search (x #f) (y #f))
  (let ([frame (playlist-search-frame #t)])
    (if (and x y)
        (send frame move x y)
        (and-let* ((f (list-ref/default (get-top-level-windows) 0 #f))
                   (x* (send f get-x))
                   (y* (send f get-y)))
          (send frame move x* y*)))
    (send frame show #t)
    (send frame focus)))
