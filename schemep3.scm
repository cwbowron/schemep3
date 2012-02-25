#lang scheme/gui

(require framework)
(require srfi/2)
(require srfi/26)

;;; local modules
(require "in-alist.ss")
(require "schemep3-status.scm")  
(require "schemep3-playlist.scm")
(require "schemep3-database.scm")
(require "schemep3-main-menu.scm")
(require "schemep3-frame-console.scm")
(require "schemep3-scrobble.scm")
(require "schemep3-helpers.scm")
(require "schemep3-backend.scm")
(require "schemep3-backend-vlc.scm")
(require "schemep3-playback.scm")
;;;(require "schemep3-backend-mplayer.scm")

(require "schemep3-twitter.scm")

;;; panels
(require "schemep3-panel-playlist-tree.scm")
(require "schemep3-panel-album-art.scm")
(require "schemep3-panel-playlist-view.scm")
(require "schemep3-panel-progress-view.scm")
(require "schemep3-panel-database-query.scm")
(require "schemep3-panel-ratings.scm")
(require "schemep3-panel-playback-control.scm")
(require "schemep3-panel-now-playing.scm")
(require "schemep3-panel-database-filter.scm")
(require "schemep3-panel-tab-playlist-switcher.scm")
(require "schemep3-mixins-gui.scm")

(require "schemep3-drop-handler.scm")
(require "schemep3-dialog-playlist-search.scm")

(preferences:set-default 'minimal-window-x 0 integer?)
(preferences:set-default 'minimal-window-y 0 integer?)

(define (menu-scan-directory)
  (and-let* ((directory
              (get-directory "Select directory" (schemep3-main-window)))
             (now (current-seconds)))
    (status:update (path->string directory))
    (schemep3-database-scan-directory directory)
    (playlist-set-from-database "last_seen>=?" now)))

(define horizontal-splitter%
  (class (checkable-panel-mixin horizontal-panel% "Horizontal Splitter")    
    (super-new 
     (stretchable-height #f))))

(define horizontal-splitter-dragable%
  (class (checkable-panel-mixin panel:horizontal-dragable% "Horizontal Splitter")    
    (super-new 
     (stretchable-height #f))))

(define vertical-splitter%
  (class (checkable-panel-mixin vertical-panel% "Vertical Splitter")
    (super-new 
     (stretchable-height #t))))

(provide/contract
 [add-children
  (-> (implementation?/c area-container<%>)
      (flat-rec-contract 
       valid-child-list
       (or/c (implementation?/c subwindow<%>)
             (list/c
              (implementation?/c area-container<%>)
              (listof valid-child-list))
             (list/c
              (implementation?/c area-container<%>)
              (listof valid-child-list)
              (or/c (listof number?)
                    false/c))))
      void)])

(define (add-children parent child)
  (cond 
    ((list? child)
     (let ((container-class (first child))
           (children (second child))
           (spacing (list-ref/default child 2 #f)))
       (let ((container (new container-class (parent parent))))
         (for ((subchild children))
           (add-children container subchild))
         (when spacing
           (send container set-percentages spacing)))))
    (else (new child (parent parent)))))

;(define-syntax ac 
;  (syntax-rules ()
;    ((_ p (x . y))
;     (let ([v (new x (parent p))])
;       (ac v y)))
;    ((_ p c)
;     (new c [parent p]))
;    ((_ p c1 z ...)
;     (begin
;       (ac p c1)
;       (ac p z ...)))))

;;;
(define base-frame%
  (class frame:basic%
    (define/public (set-app-icon icon-path type-symbol)
      (and-let* ((icon (make-object bitmap% icon-path 'png/mask))
                 ((send icon ok?)))
        (send this set-icon icon #f type-symbol)))
       
    (define/public (setup-icons)
      (send this set-app-icon "etc/schemep3-16x16.png" 'small)
      (send this set-app-icon "etc/schemep3-32x32.png" 'large))
    
    (add-pre-play-hook
     (lambda (index item)
       (let ([artist (schemep3-database-retrieve-field item 'artist)]
             [title (schemep3-database-retrieve-field item 'title)])
         (send this set-label
               (format "Schemep3 - ~A - ~A" artist title)))))
   
    (super-new
     (label "Schemep3"))
    
    (send this setup-icons)))
    
(define (show-now-playing)
  (let ([p (playlists-playing)]
        [n (now-playing-playlist-index)])
    (playlist-show n p)))

(define main-window%
  (class (frame:info-mixin (frame:size-pref-mixin base-frame%))
    
    (define/override (on-drop-file path)
      (schemep3-drop-handler:drop path))

    (frame:setup-size-pref 'main-window-size 800 600)
        
    ;;;(printf "[Creating main window] current-backend: ~A~%" (current-backend))
    
    (super-new
     (size-preferences-key 'main-window-size)
     (stretchable-width #t)
     (stretchable-height #t))
        
    (let ((area (send this get-area-container)))
      (add-children
       area
       `(,vertical-splitter%
         ((,horizontal-splitter% (,ratings-panel% ,progress-view% ,playback-control-panel%))
          ,query-panel%
          ,filter-panel%
          (,panel:vertical-dragable% ((,tab-playlist-switcher% (,playlist-view%))
                                      (,horizontal-splitter-dragable% 
                                       (,album-art-panel% ,now-playing-panel%) (1/5 4/5)))
                                     (3/5 2/5))))))
    
    (main-menu:generate (send this get-menu-bar))    
    ;;;(send (console-frame) create-menu-item this "&View" "Console")
    
    (send this accept-drop-files #t)

    (define status-message%
      (class message%
        (super-new
         (label "-------------------------------------------------------------------")           
         (stretchable-width #t))        
        (status:add-hook (cut send this set-label <>))))
    
    (define status-progress-bar%
      (class gauge%        
        (super-new
         ;;;(label "-------------------------------------------------")
         (label #f)
         (range 100))
        ;;;(send this show #f)
        (progress-bar:add-hook
         (match-lambda*
           ((list 'show)
            (let ([p (send this get-parent)])
              (unless (member this (send p get-children))
                (send p add-child this)))
            (send this show #t))
           ((list 'hide)            
            (send this show #f)
            (send (send this get-parent) delete-child this))
           ((list 'set value)
            (send this set-value value))
           ;;;((list 'set-status status)
           ;;; (send this set-label status))
           (_ (void))))))
    
    (let ((h (send this get-info-panel)))
      (add-children h status-progress-bar%)
      (add-children h playback-time-panel%)
      (add-children h status-message%)
      (send h change-children reverse))))

(define now-playing-message%
  (class message%

    (define (update . ignore)
      (and-let* ((now-playing (now-playing-database-index))
                 (artist (schemep3-database-retrieve-field now-playing 'artist))
                 (title (schemep3-database-retrieve-field now-playing 'title)))
        (send this set-label (format "~A - ~A" artist title))))
    
    (super-new
     (label "<not playing>"))
    
    (update)
    
    (add-pre-play-hook update)))

(define minimal-window%
  (class (frame:info-mixin base-frame%)
    
    (define/augment (on-close)
      (preferences:set 'minimal-window-x (send this get-x))
      (preferences:set 'minimal-window-y (send this get-y)))
    
    (super-new
     (x (preferences:get 'minimal-window-x))
     (y (preferences:get 'minimal-window-y))
     (style '(float)))

    (add-children
     (send this get-area-container)
     `(,vertical-panel%
       ((,horizontal-splitter% (,progress-view% ,playback-control-panel%)))))
        
    
    (let ((h (send this get-info-panel)))
      (new now-playing-message%
           (stretchable-width #t)
           (parent h))
      (send h change-children reverse))))

(define-lazy-singleton schemep3-main-window main-window%)
(define-lazy-singleton schemep3-mini-window minimal-window%)

(define (show-minimal-view)
  (send (schemep3-mini-window #t) show #t)
  (send/ignore (schemep3-main-window) show #f))

(define (show-normal-view)
  (send (schemep3-main-window #t) show #t)
  (send/ignore (schemep3-mini-window) show #f))

(define (setup-basic-main-menu)
  ;; file menu
  (main-menu:add main-menu:group:file (make-main-menu-item "&Scan Directory..." menu-scan-directory))
  (main-menu:add main-menu:group:file (make-main-menu-item "&Preferences..." preferences:show-dialog))
  (main-menu:add main-menu:group:file (make-main-menu-separator))
  (main-menu:add main-menu:group:file (make-main-menu-item "E&xit" exit:exit))
  ;; playback
  (main-menu:add main-menu:group:playback (make-main-menu-item "&Stop" stop))
  (main-menu:add main-menu:group:playback (make-main-menu-item "P&ause" toggle-pause))
  (main-menu:add main-menu:group:playback (make-main-menu-item "&Play" play))
  (main-menu:add main-menu:group:playback (make-main-menu-item "&Next" next))
  (main-menu:add main-menu:group:playback (make-main-menu-separator))
  (main-menu:add main-menu:group:playback (make-main-menu-item "Clear Queue" playback-queue-clear))
  (main-menu:add main-menu:group:playback (make-main-menu-separator))
  (main-menu:add main-menu:group:playback (make-main-menu-item "Show &Now Playing" show-now-playing))
  ;; playlist
  (main-menu:add main-menu:group:playlist (make-main-menu-item "&Search..." show-playlist-search))
  (main-menu:add main-menu:group:playlist (make-main-menu-item "&Clear" playlist-clear))
  (main-menu:add main-menu:group:view (make-main-menu-separator))
  ;; view
  (main-menu:add main-menu:group:view (make-main-menu-item "Minimal" show-minimal-view))
  (main-menu:add main-menu:group:view (make-main-menu-item "Normal" show-normal-view)))

(define (win32-event-loop)
  (let ([system-events-event (dynamic-require "system-events" 'system-events-event)]
        [last-system-event (dynamic-require "system-events" 'last-system-event)]
        [was-playing #f]
        [shutdown-semaphore (make-semaphore 0)]
        [win32-event-loop-thread (current-thread)])
        
    (exit:insert-on-callback-with-status
     (lambda ()
       (semaphore-post shutdown-semaphore)
       (thread-wait win32-event-loop-thread))
     "shutdown win32-event-loop")
    
    (let internal-loop ()
      (let ([event (sync system-events-event shutdown-semaphore)])
        (unless (eq? event shutdown-semaphore)
          (case (last-system-event)
            ((HOTKEY-PAUSE) (toggle-pause))
            ((HOTKEY-NEXT) (next))
            ((CONSOLE-LOCK)
             (set! was-playing (playing?))
             (when was-playing
               (toggle-pause)))
            ((CONSOLE-UNLOCK)
             (when (and was-playing (paused?))
               (toggle-pause))))
          (internal-loop))))))

(define (main)
  (exit:insert-on-callback-with-status schemep3-database-close "schemep3-database-close")
  (exit:insert-on-callback-with-status stop "playback-stop")
  (application:current-app-name "Schemep3")

  (setup-basic-main-menu)
  (playlists-create "Default")
  
  (when (equal? (system-type) 'windows)
    (thread win32-event-loop))
    
  (add-post-play-hook
   (lambda (playlist-index file-index elapsed elapsed-percent)
     (when (or (> elapsed 60)
               (> elapsed-percent 50))
       (schemep3-database-increase-play-count file-index)
       (schemep3-database-update-field file-index 'last_played (current-seconds)))))
  
  (show-normal-view)
  
  (progress-bar:hide)
  )

;;;;;;;;;;;;;;;;;;
(current-backend (new vlc-backend%))
(preview-backend (new vlc-backend%))

(let/ec k
  (call-with-exception-handler
   (lambda (x)
     (error "Error Bitches: ~A~%" x)
     (k))
   main   
   ;;;(lambda () (thread main))))
   ))