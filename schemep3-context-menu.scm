#lang scheme/gui

(define list-of-file-indexes/c (listof database-index/c))
(define list-of-playlist-indexes/c (listof valid-playlist-index/c))

(provide/contract
 [move-selected-playlist-items-up   (-> list-of-playlist-indexes/c void)]
 [move-selected-playlist-items-down (-> list-of-playlist-indexes/c void)]
 
 [context-menu-rate-1               (-> list-of-file-indexes/c void)]
 [context-menu-rate-2               (-> list-of-file-indexes/c void)]
 [context-menu-rate-3               (-> list-of-file-indexes/c void)]
 [context-menu-rate-4               (-> list-of-file-indexes/c void)]
 [context-menu-rate-5               (-> list-of-file-indexes/c void)]
 [rate-items                        (-> (between/c 0 5) list-of-file-indexes/c void)]
 [context-menu-open-folder          (-> list-of-file-indexes/c void)]
 )

(provide
;; ASIN
;; show-ASIN
 open-folder
 )

(require scheme/date)
(require net/sendurl)
(require srfi/2)
(require srfi/26)
(require srfi/54)

(require "schemep3-helpers.scm")
(require "schemep3-playback.scm")
(require "schemep3-database.scm")
(require "schemep3-playlist.scm")
(require "schemep3-status.scm")
(require "schemep3-file-mover.scm")
(require "schemep3-dialog-properties.scm")
(require "schemep3-context-menu-manager.scm")
(require "schemep3-frame-console.scm")

(require "taglib.ss")
;;(require "taglib-extended.scm")

(define (open-url url-string)
  (send-url url-string))

(define (open-folder path)
  (shell-execute #f path "" path 'sw_show))


;;; context menu 
(define (update-last-played db-index filename extended-tags)
  (and-let* ((file-last-played-pair (assoc "PLAY_DATE" extended-tags))
             (file-last-played (cdr file-last-played-pair))
             (file-played-seconds (timestamp->seconds file-last-played)))
    (let ((db-last-played (schemep3-database-retrieve-field db-index 'last_played)))
      (unless db-last-played
        (schemep3-database-update-field db-index 'last_played file-played-seconds)))))

(define (update-play-count db-index filename extended-tags)
  (and-let* ((play-count-pair (assoc "PLAY_COUNTER" extended-tags))
             (play-count-string (cdr play-count-pair))
             (play-count (string->number play-count-string)))
    (let ((db-count (schemep3-database-retrieve-field db-index 'play_count)))
      (when (< db-count play-count)
        (let ((new-count (+ db-count play-count)))
          (console:printf "Updating ~A to ~A~%" filename new-count)
          (schemep3-database-update-field db-index 'play_count new-count))))))
  
;(define (update-extended-fields db-index)
;  (let ((filename (schemep3-database-index->filename db-index)))
;    (and-let* ((extended-tags (taglib-extended-tags filename)))
;      (update-last-played db-index filename extended-tags)
;      (update-play-count db-index filename extended-tags))))

(define (context-menu-reload-tags items (force-update? #f))
  (for ((item items))
    (schemep3-database-reload-item item force-update?)
    ;;;(update-extended-fields item)
    ))

(define (context-menu-reload-tags-force items)
  (context-menu-reload-tags items #t))

(define (write-m3u path file-list)
  (let ((out-port (open-output-file path #:mode 'binary #:exists 'replace)))
    (for ((file file-list))
      (fprintf out-port "~A~%" (if (path? file) (path->string file) file)))
    (close-output-port out-port)))

;;;; playlist related commands
(define (move-playlist-items indices offset)
  (for ((n indices))
    (playlist-swap-items n (+ n offset))))

(define (move-selected-playlist-items-up playlist-indexes)
  (when (> (apply min playlist-indexes) 0)
    (move-playlist-items playlist-indexes -1)
    (playlist-select 
     (map sub1 playlist-indexes))))

(define (move-selected-playlist-items-down playlist-indexes)
  (let ((indices (sort playlist-indexes >))
        (count (playlist-item-count)))
    (when (< (apply max indices) (- count 1))
      (move-playlist-items indices 1)
      (playlist-select 
       (map add1 indices)))))

(define (remove-selected-playlist-items playlist-indexes)
  (let ((indices (sort playlist-indexes >)))
    (for ((index indices))
      (playlist-remove index))))

(define (context-menu-play-next playlist-indexes)
  (playback-queue-add playlist-indexes #t))

(define (context-menu-add-to-queue playlist-indexes)
  (playback-queue-add playlist-indexes #f))

;;;;;;;;;; end playlist commands

;(define (ASIN path)
;  (and-let* ([extended-tags (taglib-extended-tags path)])
;    (find-tag extended-tags "ASIN")))
;
;(define (show-ASIN asin)
;  (and-let* ([url (string-append "http://amazon.com/gp/product/" asin)])
;    (open-url url)))

;;;(define (show-on-amazon path)
;;;  (and-let* ((asin (ASIN path)))
;;;    (show-ASIN asin)))

;(define (gen-tag-specific-options parent-menu items)
;  (when (for/or ((item items))
;          (ASIN (schemep3-database-index->filename item)))
;    (new menu-item%
;         [parent parent-menu]
;         [label "Find on Amazon"]
;         [callback
;          (lambda (menu event)
;            (for ((item items))
;              (and-let* ((asin (ASIN (schemep3-database-index->filename item))))
;                (show-ASIN asin))))])))

(define (context-menu-export-m3u items)
  (and-let* ((output-file 
              (put-file 
               "Select output file" 
               #f #f #f
               "m3u"
               (list)
               '(("m3u" "*.m3u")))))
    (status:update "Export ~a" output-file)
    (write-m3u output-file (map schemep3-database-index->filename items))))

(define (context-menu-move-to-library items)
  (confirm-and-move-files
   (map schemep3-database-index->filename items)))

(define (context-menu-properties items)
  (new properties-frame% 
       (parent #f)
       (items items)))

(define (rate-items rating items)
  (for ((db-index items))
    (schemep3-database-update-field db-index 'rating rating)))

(define context-menu-rate-1 (cut rate-items 1 <>))
(define context-menu-rate-2 (cut rate-items 2 <>))
(define context-menu-rate-3 (cut rate-items 3 <>))
(define context-menu-rate-4 (cut rate-items 4 <>))
(define context-menu-rate-5 (cut rate-items 5 <>))

(define (context-menu-preview items)
  (for ((item items))
    (preview item)))

(define (context-menu-open-folder items)
  (for ((item items))
    (let ([p (path->string (path-only (schemep3-database-index->filename item)))])
      (open-folder p))))

(define (context-menu-search-album-art items)
  (for ((item items))
    (let ([album (schemep3-database-retrieve-field item 'album)]
          [artist (schemep3-database-retrieve-field item 'artist)])
      (let ([url (format "http://images.google.com/images?hl=en&q=~A+~A" album artist)])
        (open-url url)))))

;;; remove dead entries
(define (psychopomp file-indexes)
  (for ((f (reverse file-indexes)))
    (let ((fn (schemep3-database-index->filename f)))
      (unless (file-exists? fn)
        (schemep3-database-remove f)))))

(define (gen-add-to-playlist parent-menu items)
  (let ([m (new menu% 
                [parent parent-menu]
                [label "Add to Playlist"])])                
    (for ((n (in-range (playlists-count))))
      (new menu-item%
           [parent m]
           [label (playlists-name n)]
           [callback
            (lambda (menu event)
              (playlist-add items n))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(playlist-operations:add "Remove" remove-selected-playlist-items)
(playlist-operations:add "Add to queue" context-menu-add-to-queue)
(playlist-operations:add "Play next" context-menu-play-next)

(file-operations:add (make-file-operation-generator gen-add-to-playlist))

(file-operations:add (make-file-operation-group 
                      "Move / Copy"
                      (list (make-file-operation "Move to library..." context-menu-move-to-library)
                            (make-file-operation "Copy to mini library..." mini-library:copy)
                            (make-file-operation "Create mp3 disc source..." mp3-disc:create)
                            (make-file-operation "Create Archos Source..." archos:copy)
                            )))
                     
(file-operations:add (make-file-operation "Reload tags" context-menu-reload-tags))
(file-operations:add (make-file-operation "Reload Tags [Force]" context-menu-reload-tags-force))
(file-operations:add (make-file-operation "Export m3u..." context-menu-export-m3u))
;;(file-operations:add (make-file-operation-generator gen-tag-specific-options))
(file-operations:add (make-file-operation "Open Folder" context-menu-open-folder))
(file-operations:add (make-file-operation "Search Album Art" context-menu-search-album-art))
(file-operations:add (make-file-operation "Psychopomp" psychopomp))

(file-operations:add (make-file-operation-group
                      "Rating" 
                      (list (make-file-operation "1" context-menu-rate-1)
                            (make-file-operation "2" context-menu-rate-2)
                            (make-file-operation "3" context-menu-rate-3)
                            (make-file-operation "4" context-menu-rate-4)
                            (make-file-operation "5" context-menu-rate-5))))

(file-operations:add (make-file-operation "Preview" context-menu-preview))
(file-operations:add (make-file-operation "Properties..." context-menu-properties))
