#lang scheme/gui

(provide 
 playlist-view%
 playback-queue-view%
 )

(require scheme/date)
(require framework)
(require srfi/2)
(require srfi/26)
(require srfi/54)
(require (planet untyped/unlib/list))

;;; local modules
(require "schemep3-playback.scm")
(require "schemep3-database.scm")
(require "schemep3-playlist.scm")
(require "schemep3-dialog-playlist-search.scm")
(require "schemep3-context-menu-manager.scm")
(require "schemep3-context-menu.scm")
(require "schemep3-helpers.scm")
(require "schemep3-gui-helpers.scm")
(require "schemep3-mixins-gui.scm")

(define (database-index->filename-only file-index)
  (path->string 
   (file-name-from-path 
    (schemep3-database-index->filename file-index))))

(define (database-index->containing-folder file-index)
  (let ((filename (schemep3-database-index->filename file-index)))
    (let ((explosion (explode-path filename)))
      (path->string
       (list-ref explosion (- (length explosion) 2))))))

(define (days-ago now then)
  (- (date->julian/scalinger now)
     (date->julian/scalinger then)))

(define (date->my-hour dt)
  (let ([h (modulo (date-hour dt) 12)])
    (if (zero? h) 12 h)))

(define (date->time dt)
  (format "~A:~A~A" 
          (cat (date->my-hour dt) 2 #\space)
          (cat (date-minute dt) 2 #\0)
          (if (< (date-hour dt) 12) "am" "pm")))

(define (format-last-played lp)
  (let ([lp-date (seconds->date lp)]
        [now-date (seconds->date (current-seconds))])
    (let ([n-days (days-ago now-date lp-date)])
      (cond [(zero? n-days) 
             (date->time lp-date)]
            [(= 1 n-days) "Yesterday"]
            [(< n-days 7)
             (format "~A days ago" n-days)]
            [else (format-date lp-date)]))))

(define playlist-view-base%
  (class list-box%
    
    (define refresh-thread #f)
    
    (define _current-mode 'active)
    
    ;;; integer or one of ('active 'playing 'queue)
    (define/public (current-mode (new-mode #f))
      (when (and new-mode (not (eq? new-mode _current-mode)))
        (set! _current-mode new-mode)
        (case _current-mode
          ((queue)
           (local-set-to-queue))
          (else
           (local-set-contents (playlist-contents (current-list))))))
      _current-mode)
    
    (define/public (current-list)
      (let ((mode (current-mode)))
        (case mode
          ((queue) 'queue)
          ((active) (playlists-active))
          ((playing) (playlists-playing))
          (else mode))))
    
    (define/private (affects-us? playlist)
      (let ((mode (current-mode)))
        (or (eq? playlist mode)
            (and (eq? mode 'active)
                 (= playlist (playlists-active)))
            (and (eq? mode 'playing)
                 (= playlist (playlists-playing)))
            (eq? (current-list) playlist))))
    
    (define (clear-pending)
      (thread-send-and-wait refresh-thread 'quit))
    
    (define (wait-pending)
      (and-let* ((t refresh-thread))
        (thread-wait t)))
        
    (define (format-column-item fmt file-index)
      (let* ((len (playlist-format-item-length fmt))
             (field (playlist-format-item-field fmt))
             (field-value (schemep3-database-retrieve-field file-index field))
             [str (cond
                    ((and field-value (playlist-format-item-format-function fmt))
                     => (cut <> field-value))
                    (field-value 
                     (ensure-string field-value))
                    ((playlist-format-item-default-function fmt)
                     => (cut <> file-index))
                    (else ""))])
        (padded-string 
         (gui-utils:trim-string str (abs len))
         len)))
    
    (define-struct playlist-format-item
      (field length format-function default-function)
      #:property prop:procedure format-column-item)
    
    (define (make-column field length #:formatter (formatter #f) #:otherwise (otherwise #f))
      (make-playlist-format-item field length formatter otherwise))
    
    (define playlist-format-list
      (list (make-column 'duration      5 #:formatter format-time)
            (make-column 'rating        1)
            (make-column 'artist      -20)
            (make-column 'title       -30 #:otherwise database-index->filename-only)
            (make-column 'album       -30 #:otherwise database-index->containing-folder)
            (make-column 'play_count    2)
            (make-column 'last_played  10 #:formatter format-last-played)))
    
    (define/private (display-string playlist-index file-index)
      (let ([base-string 
             (string-join (map (cut <> file-index) playlist-format-list) " ")])
        (decorate-if-playing
         (decorate-if-queue base-string file-index)
         playlist-index file-index)))
    
    (define/private (local-clear)
      (clear-pending)
      (send this set (list)))
    
    (define/private (local-add-item db-index)
      (let* ([n (send this get-number)]
             [string (display-string n db-index)])
        (send this append string db-index)))
        
    (define/private (non-threaded-set-contents file-indexes)
      (send this set (for/list (((file-index n) (in-indexed file-indexes)))
                       (display-string n file-index)))
      (let* ((element-count (send this get-number)))
        (for (((f n) (in-indexed file-indexes)))
          (send this set-data n f))))        
        
    (define/private (threaded-set-contents file-indexes)
      (send this set (map number->string file-indexes))
      (let* ((element-count (send this get-number)))
        (for (((f n) (in-indexed file-indexes)))
          (send this set-data n f))
        (thread-set! refresh-thread
                     (lambda ()
                       (for/or ((n (in-range element-count)))
                         (refresh-item n)
                         (thread-try-receive))))))

    (define/private (local-set-contents file-indexes)
      (clear-pending)
      (if (< (length file-indexes) 500)
          (non-threaded-set-contents file-indexes)
          (threaded-set-contents file-indexes)))
        
    (define highlited-items (list))
    
    (define/private (decorate-if-queue string file-index)
      (if (playback-queue-member-file-index? file-index)
          (string-append string " Q")
          string))
    
    (define/private (decorate-if-playing string playlist-index file-index)
      (cond 
        [(and (eq? file-index (now-playing-database-index))
              (eq? (playlists-playing) (current-list))
              (eq? playlist-index (now-playing-playlist-index)))
         (push! highlited-items playlist-index)
         (regexp-replace* " " string "=")]
        [(and (eq? file-index (now-playing-database-index)))
         (push! highlited-items playlist-index)
         (regexp-replace* " " string "-")]
        [else string]))
          
    (define/public (refresh-item playlist-index)
      (and-let* (((< playlist-index (send this get-number)))
                 (file-index (send this get-data playlist-index))
                 (string (display-string playlist-index file-index)))
        (send this set-string playlist-index string)))

    (define/private (local-set-to-queue)
      (local-set-contents
       (map third (playback-queue-contents))))
    
    (define/public (ensure-visible n)
      (let ((i (send this get-first-visible-item))
            (j (send this number-of-visible-items)))
        (cond
          ((>= i n)
           (send this set-first-visible-item n))
          ((>= n (+ i j))
           (send this set-first-visible-item (+ (- n j) 1))))))
    
    (define/public (index-from-position x y)
      (let ((n (send this number-of-visible-items))
            (offset (send this get-first-visible-item))
            (height (send this get-height)))
        (let ((item-height (/ height n)))
          (+ offset (floor (/ y item-height))))))
    
    (define keymap
      `((#\5 . ,(cut context-menu-rate-5 (get-selection-data)))
        (#\4 . ,(cut context-menu-rate-4 (get-selection-data)))
        (#\3 . ,(cut context-menu-rate-3 (get-selection-data)))
        (#\2 . ,(cut context-menu-rate-2 (get-selection-data)))
        (#\1 . ,(cut context-menu-rate-1 (get-selection-data)))
        (#\n . ,next)
        (#\N . ,next)
        (#\f . ,show-playlist-search)
        (#\space . ,toggle-pause)))
    
    (define alt-keymap
      `((up   . ,(cut move-selected-playlist-items-up (playlist-selected-playlist-indexes)))
        (down . ,(cut move-selected-playlist-items-down (playlist-selected-playlist-indexes)))))
    
    (define (call-and-return-true fn)
      (fn)
      #t)
    
    (define/override (on-subwindow-char receiver event)
      (let ((keycode (send event get-key-code)))
        (cond 
          [(assoc-value/default keycode keymap #f) => call-and-return-true]
          [(and
            (send event get-meta-down)
            (assoc-value/default keycode alt-keymap #f)) => call-and-return-true]
          [else (super on-subwindow-char receiver event)])))
    
    (define/private (get-selection-data)
      (map (cut send this get-data <>)
           (send this get-selections)))
    
    (define (item-context-menu x y)
      (when (<= (length (send this get-selections)) 1)
        (let ((p-index (send this index-from-position x y)))
          (when (< p-index (send this get-number))
            (send this set-selection p-index)
            (playlist-select p-index))))
      (let ((pm (new popup-menu%)))
        (if (eq? (current-mode) 'queue)
            (generate-context-menu pm (get-selection-data))
            (generate-context-menu pm))
        (send this popup-menu pm x y)))

    (define (playlist-selector-context-menu x y)
      (let ((m (new popup-menu%)))
        (new checkable-menu-item%
             (parent m)
             (label "<Active>")
             (callback (lambda (m e) (current-mode 'active)))
             (checked (eq? (current-mode) 'active)))
        (new checkable-menu-item%
             (parent m)
             (label "<Playing>")
             (callback (lambda (m e) (current-mode 'playing)))
             (checked (eq? (current-mode) 'playing)))
        (new checkable-menu-item%
             (parent m)
             (label "<Queue>")
             (callback (lambda (m e) (current-mode 'queue)))
             (checked (eq? (current-mode) 'queue)))
        (new separator-menu-item% (parent m))        
        (for ((n (in-range (playlists-count))))
          (new checkable-menu-item%
               [parent m]
               [label (format "~A - ~A" n (playlists-name n))]
               [callback (lambda (m e) (current-mode n))]
               [checked (eq? (current-mode) n)]))
        (send this popup-menu m x y)))
    
    (define/override (on-subwindow-event receiver event)
      (case (send event get-event-type)
        ((right-down)
         (item-context-menu (send event get-x) (send event get-y))
         #t)
        ((middle-down)
         (playlist-selector-context-menu (send event get-x) (send event get-y))
         #t)
        (else (super on-subwindow-event receiver event))))
    
    (define/private (find-item file-index)
      (for/list ((n (in-range (send this get-number)))
                 #:when (and-let* ((z (send this get-data n)))
                          (= z file-index)))
        n))
    
    (define/private (clear-selection)
      (for ((selected-index (send this get-selections)))
        (send this select selected-index #f)))
    
    (define/private (set-selection-list new-selections)
      (let ((current-selections (send this get-selections)))
        (unless (equal? current-selections new-selections)
          (clear-selection)
          (for ((new-selection new-selections))
            (send this select new-selection #t)))))
    
    (define/public (on-double-click)
      (stop)
      (playback-queue-add (send this get-selections) #t (current-list))
      (play))
      
    (super-new
     (label #f)
     (choices (list))
     (style '(multiple))
     (callback 
      (lambda (lb e)
        (unless (eq? (current-mode) 'queue)
          (playlist-select 
           (send this get-selections)))
        (cond 
          ((equal? (send e get-event-type) 'list-box-dclick)
           (send lb on-double-click)))))
     (font (get-medium-mono-font)))
    
    (exit:insert-on-callback-with-status clear-pending "playlist-view::clear-pending")
    
    (define (refresh-queue-items Q)
      (let ([our-list (current-list)])
        (for ((queue-item Q)
              #:when (= our-list (second queue-item)))
          (send this refresh-item (first queue-item)))))
    
    (playlist-add-hook
     (match-lambda*
       ((list 'add items playlist)
        (when (affects-us? playlist)
          (for ((item items))
            (local-add-item item))))
       ((list 'remove index playlist)
        (when (affects-us? playlist)
          (wait-pending)
          (send this delete index)))
       ((list 'clear playlist)
        (when (affects-us? playlist)
          (local-clear)))
       ((list 'set-item index value playlist)
        (when (affects-us? playlist)
          (send this set-data index value)
          (send this refresh-item index)))
       ((list 'set playlist)
        (when (affects-us? playlist)
          (local-set-contents (playlist-contents playlist))))
       ((list 'playback-queue-clear previous-queue)
        (if (eq? (current-mode) 'queue)
            (local-clear)
            (refresh-queue-items previous-queue)))
       ((list 'playback-queue-add)
        (if (eq? (current-mode) 'queue)
            (local-set-to-queue)
            (refresh-queue-items (playback-queue-contents))))
       ((list 'select items)
        (when (affects-us? (playlists-active))
          (set-selection-list items)))
;;       ((list 'playlists-delete index)
;;        (cond
;;          [(eq? (current-mode) 'queue) #t]
;;          [(or
;;            (eq? (current-mode) 'active)
;;            (eq? (current-mode) 'playing))
;;           (local-set-contents (playlist-contents (current-list)))]
;;          [(>= index (current-mode))
;;           (current-mode (sub1 index))]))
       ((list 'playlists-set-playing p-index)
        (when (eq? (current-mode) 'playing)
          (local-set-contents (playlist-contents p-index))))
       ((list 'playlists-set-active p-index)
        (when (eq? (current-mode) 'active)
          (local-set-contents (playlist-contents p-index))))
       ((list 'show n p)
        (when (eq? (current-list) p)
          (send this ensure-visible n)))
       (_ (void))))
         
    (schemep3-database-add-update-hook
     (lambda (file-index)
       (for ((n (find-item file-index)))
         (refresh-item n))))
    
    (add-pre-play-hook
     (lambda (index item)
       (when (affects-us? (playlists-playing))
         (refresh-item index))))
    
    (add-post-play-hook
     (lambda x
       (let ([refresh-items highlited-items])
         (set! highlited-items (list))
         (for ((item refresh-items))
           (refresh-item item)))))
    
    (add-post-play-hook
     (lambda x
       (when (eq? (current-mode) 'queue)
         (local-set-to-queue))))))

(define playlist-view%
  (class (checkable-panel-mixin playlist-view-base% "Playlist")
    (super-new)))

(define playback-queue-view%
  (class (checkable-panel-mixin playlist-view-base% "Queue Viewer")
    (super-new)
    (send this current-mode 'queue)))
