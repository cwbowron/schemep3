#lang scheme/gui

(provide fancy-playlist-view%)

(require mrlib/hierlist)

(require srfi/2)

;;; local modules
(require "schemep3-playback.scm")
(require "schemep3-database.scm")
(require "schemep3-playlist.scm")
(require "schemep3-context-menu.scm")
(require "schemep3-helpers.scm")
(require "schemep3-gui-helpers.scm")
(require "schemep3-main-menu.scm")
(require "schemep3-mixins-gui.scm")

(define fancy-playlist-view%
  (class (checkable-panel-mixin hierarchical-list% "Playlist")
    
    (define _playlist-item-format
      `((5 "duration" ,format-time)
        (1 "rating")           
        (-20 "artist")
        (-30 "title" #f
             ,(lambda (index) 
                (path->string 
                 (file-name-from-path (schemep3-database-index->filename index)))))
        (-30 "album" #f
             ,(lambda (index)
                (let ((filename (schemep3-database-index->filename index)))
                  (let ((explosion (explode-path filename)))
                    (path->string
                     (list-ref explosion (- (length explosion) 2)))))))
        (2 "play_count")
        (10 "last_played" ,format-date)))
        
    (define/private (item-string-from-index index)
      (string-join
       (for/list ((column-format _playlist-item-format))
         (let ((column-length (first column-format))
               (column-field (second column-format))
               (column-proc 
                (and (>= (length column-format) 3) (third column-format)))
               (else-proc
                (and (>= (length column-format) 4) (fourth column-format))))
           (padded-string 
            (let ((field-value (schemep3-database-retrieve-field index column-field)))
              (ensure-string
               (cond
                 ((and field-value column-proc)
                  (column-proc field-value))
                 (field-value field-value)
                 (else-proc
                  (else-proc index))
                 (else ""))))
            column-length)))
       " "))
    
;    (define/private (local-add-item db-index)
;      (send this append "" db-index)
;      (refresh-item (- (send this get-number) 1)))
    
;    (define/public (refresh-item playlist-index (stopping? #f))
;      (when (< playlist-index (send this get-number))
;        (let* ((file-index 
;                (send this get-data playlist-index)))
;          (let ((string (item-string-from-index file-index)))
;            (when (playback-queue-member? playlist-index)
;              (set! string (string-append string " Q")))
;            (when (and (not stopping?)
;                       (equal? file-index (now-playing 'database-index)))
;              (set! string (regexp-replace* " " string "=")))
;            (send this set-string playlist-index string)))))
;    
;    (define/public (ensure-visible n)
;      (let ((i (send this get-first-visible-item))
;            (j (send this number-of-visible-items)))
;        (cond
;          ((>= i n)
;           (send this set-first-visible-item n))
;          ((>= n (+ i j))
;           (send this set-first-visible-item (+ (- n j) 1))))))
;    
;    (define/public (index-from-position x y)
;      (let ((n (send this number-of-visible-items))
;            (offset (send this get-first-visible-item))
;            (height (send this get-height)))
;        (let ((item-height (/ height n)))
;          (+ offset (floor (/ y item-height))))))
;    
;    (define/override (on-subwindow-char receiver event)
;      (unless (case (send event get-key-code)
;                ((up)
;                 (if (send event get-meta-down)           
;                     (begin
;                       (move-selected-playlist-items-up)
;                       #t)
;                     #f))
;                ((down)
;                 (if (send event get-meta-down)           
;                     (begin
;                       (move-selected-playlist-items-down)
;                       #t)
;                     #f))
;                ((#\5) (context-menu-rate-5))
;                ((#\4) (context-menu-rate-4))
;                ((#\3) (context-menu-rate-3))
;                ((#\2) (context-menu-rate-2))
;                ((#\1) (context-menu-rate-1))
;                ((#\n #\N)
;                 (next)
;                 #t)
;                ((#\space)
;                 (pause)
;                 #t)
;                (else #f))
;        (super on-subwindow-char receiver event)))
        
;    (define/override (on-subwindow-event receiver event)
;      (if (send event button-down? 'right)
;          (begin
;            (when (<= (length (send this get-selections)) 1)
;              (let ((p-index (send this index-from-position
;                                   (send event get-x)
;                                   (send event get-y))))
;                (send this set-selection p-index)
;                (playlist-select p-index)))
;            (let ((pm (new popup-menu%)))              
;              (generate-context-menu pm context-menu-items)
;              (send this 
;                    popup-menu
;                    pm
;                    (send event get-x)
;                    (send event get-y))))
;          (super on-subwindow-event receiver event)))
    
;    (define/private (find-item file-index)
;      (let find-item-2 ((n 0) (a (list)))
;        (cond
;          ((>= n (send this get-number)) a)
;          ((= (send this get-data n) file-index)
;           (find-item-2 (+ n 1) (cons n a)))
;          (else (find-item-2 (+ n 1) a)))))
    
;    (define/private (clear-selection)
;      (for ((selected-index (send this get-selections)))
;        (send this select selected-index #f)))
;    
;    (define/private (set-selection-list new-selections)
;      (let ((current-selections (send this get-selections)))
;        (unless (equal? current-selections new-selections)
;          (clear-selection)
;          (for ((new-selection new-selections))
;            (send this select new-selection #t)))))
    
    (super-new
;;;     (label #f)
     ;;;(choices (list))
     ;;(style '(multiple))
;     (callback 
;      (lambda (lb e)
;        (playlist-select (send this get-selections))
;        (cond 
;          ((equal? (send e get-event-type) 'list-box-dclick)
;           (stop)
;           (play)))))
;     (font (get-medium-mono-font))
     )
    
;    (schemep3-database-add-hook
;     (lambda (file-index)
;       (let ((playlist-indexes (find-item file-index)));
;         (for ((playlist-index playlist-indexes))
;           (refresh-item playlist-index)))))
    
    (playlist-add-hook
     (lambda (op-type . params)       
       (case op-type
         ((add)
          (for ((item-index (first params)))
            (let ((playlist-item (send this new-item)))
              
              (for ((field (list "artist" "title" "album"))
                    (offset (list 20 20 20))
                    (max-length (list 19 19 19)))
                (let ((value (schemep3-database-retrieve-field item-index field)))
                  (send (send playlist-item get-editor) insert value)
                  (send (send playlist-item get-editor) insert (make-object tab-snip%))))
              (send playlist-item user-data item-index))))
;         ((remove)
;          (let ((index (first params)))
;            (send this delete index)))
         ((clear)
          (for ((x (send this get-items)))
            (send this delete-item x)))
;         ((set-item)
;          (let ((index (first params))
;                (value (second params)))
;            (send this set-data index value)
;            (send this refresh-item index)))
;         ((playback-queue-add)
;          (for ((playlist-index (playback-queue-contents)))
;            (send this refresh-item playlist-index)))
;         ((select)
;          (let ((items (first params)))
;            (set-selection-list items)))
;         ((show-now-playing)
;          (and-let* ((np (now-playing 'playlist-index)))
;            (send this ensure-visible np)))
         (else 
          (printf "Playlist Hook Called: ~A~%" op-type)))))
    
;    (add-pre-play-hook
;     (lambda (index item)
;       (refresh-item index)))
    
;    (add-post-play-hook
;     (lambda (index item)        
;       (refresh-item index #t)))
    ))

