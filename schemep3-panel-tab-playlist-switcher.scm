#lang scheme/gui

(provide tab-playlist-switcher%)

(require srfi/2)

(require "schemep3-mixins-gui.scm")
(require "schemep3-playlist.scm")

(require mred/private/const)

(define tab-playlist-switcher%
  (class (checkable-panel-mixin tab-panel% "Playlist Tab Switcher")
    
    (define (new-playlist-callback control event)
      (playlists-active (playlists-create)))
    
    (define (remove-playlist-callback control event)
      (playlists-delete (send this get-selection)))
    
    (define (rename-playlist-callback control event)
      (let ((current-text (get-selected-label))
            (current-index (send this get-selection)))
        (and-let* ((new-text
                    (get-text-from-user "New Title" "New playlist title" #f current-text)))
          (playlists-name current-index new-text))))
    
    (define/public (get-selected-label)
      (send this get-item-label (send this get-selection)))
    
    (define/private (do-local-context-menu x y)
      (let ((pm (new popup-menu%)))
        (new menu-item% 
             (parent pm)
             (callback new-playlist-callback)
             (label "New Playlist"))
        (new menu-item%
             (parent pm)
             (callback remove-playlist-callback)
             (label (format "Remove '~A'" (get-selected-label))))
        (new menu-item%
             (parent pm)
             (callback rename-playlist-callback)
             (label (format "Rename '~A'" (get-selected-label))))
        (send this 
              popup-menu
              pm
              x
              y)))

    (define/override (on-subwindow-event control event)
      (if (and 
           (send event button-down? 'right)
           (or 
            (is-a? control wx-tab-group<%>)
            (eq? control this)))
          (begin 
            (do-local-context-menu (send event get-x) (send event get-y))
            #t)
          (super on-subwindow-event control event)))
            
    (super-new
     ;;;(min-height 0)
     ;;;(stretchable-height #f)
     (callback 
      (lambda (control event)
        (playlists-active (send this get-selection))))
     (choices 
      (for/list ((n (in-range (playlists-count))))
        (playlists-name n))))
    
    (define (set-active n)
      (unless (= n (send this get-selection))
        (send this set-selection n)))
    
    (playlist-add-hook
     (match-lambda*
       ((list 'playlists-create name)
        (send this append name))
       ((list 'playlists-delete index)
        (send this delete index)
        (set-active (playlists-active)))
       ((list 'playlists-rename name index)
        (send this set-item-label index name))
       ((list 'playlists-set-active index)
        (set-active index))
       (_ (void))))))
