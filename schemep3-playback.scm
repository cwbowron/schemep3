#lang scheme/gui

(provide/contract
 [stop                   (-> void)]
 [play                   (-> void)]
 [toggle-pause           (-> void)]
 [next                   (-> void)]
 [paused?                (-> boolean?)]
 [playing?               (-> boolean?)]
 [seek                   (-> (between/c 0 100) void)] 
 [preview                (-> database-index/c void)]
 
 [now-playing-database-index (-> (or/c #f database-index/c))]
 [now-playing-playlist-index (-> (or/c #f valid-playlist-index/c))]
 
 [playback-supported?    (-> path-string? boolean?)]

 [add-pre-play-hook      (-> procedure? void)]
 [add-post-play-hook     (-> procedure? void)]
 [add-progress-hook      (-> procedure? void)]
 )

(require framework)
(require srfi/2)

(require "schemep3-playlist.scm")
(require "schemep3-database.scm")
(require "schemep3-helpers.scm")
(require "schemep3-backend.scm")
(require "schemep3-mixins-gui.scm")

(define preview-duration (preferences-backed-variable 'schemep3-preview-duration 15))

(define _current-track-database-index #f)
(define _current-track-playlist-index #f)

(define _continue-playback-loop #f)

(define playback-state #f)
(define playback-thread #f)

(define (set-playback-state new-state)
  (set! playback-state new-state))

(define-unnamed-hook add-pre-play-hook  playback:call-hooks:pre-play)
(define-unnamed-hook add-post-play-hook playback:call-hooks:post-play)
(define-unnamed-hook add-progress-hook  playback:call-hooks:progress)

;(define (playback:internal-next-item)
;  (let ([Q (playback-queue-contents)])
;    (cond
;      ((not (null? Q))
;       (let ([head (car Q)])
;         (let ((Q-item-playlist-index (first head))
;               (Q-item-playlist-number (second head))
;               (Q-item-file-index (third head)))
;           (playback-queue-advance)
;           (playlists-playing Q-item-playlist-number)
;           (values Q-item-file-index Q-item-playlist-index))))
;      (else
;       (let* ([selected-indexes (playlist-selected-playlist-indexes)]
;              [p-index
;               (cond [_current-track-playlist-index
;                      (add1 _current-track-playlist-index)]
;                     [(not (null? selected-indexes))
;                      (playlists-playing (playlists-active))
;                      (car selected-indexes)]
;                     [else 0])])
;         (if (< p-index (playlist-item-count (playlists-playing)))
;             (values (playlist-item p-index (playlists-playing)) p-index)
;             (values #f #f)))))))

(define (playback:internal-next-item)
  (let ([queue-item (playback-queue:next!)])
    (if queue-item
        (let ((Q-item-playlist-index (first queue-item))
              (Q-item-playlist-number (second queue-item))
              (Q-item-file-index (third queue-item)))
          (playlists-playing Q-item-playlist-number)
          (values Q-item-file-index Q-item-playlist-index))
        (let* ([selected-indexes (playlist-selected-playlist-indexes)]
               [p-index
                (cond [_current-track-playlist-index
                       (add1 _current-track-playlist-index)]
                      [(not (null? selected-indexes))
                       (playlists-playing (playlists-active))
                       (car selected-indexes)]
                      [else 0])])
          (if (< p-index (playlist-item-count (playlists-playing)))
              (values (playlist-item p-index (playlists-playing)) p-index)
              (values #f #f))))))

(define (playback:next-item)
  (set!-values (_current-track-database-index _current-track-playlist-index)
               (playback:internal-next-item))
  (values _current-track-database-index _current-track-playlist-index))

(define (playback-loop)
  (let-values ([(file-index playlist-index) (playback:next-item)])
    (when file-index
      (let* ((file (schemep3-database-index->filename file-index)))
        (when (file-exists? file)
          (set-playback-state 'playing)
          (playback:call-hooks:pre-play playlist-index file-index)
          (let-values ([(time position)
                        (send (current-backend) play file playback:call-hooks:progress)])
            (set-playback-state 'stopped)
            (playback:call-hooks:post-play playlist-index file-index
                                           (or time 0)
                                           (or position 0)))))
      (when _continue-playback-loop
        (playback-loop)))))

(define-syntax with-paused-playback
  (syntax-rules ()
    ((_ body ...)
     (let ([p (playing?)])
       (when p
         (toggle-pause))
       body ...
       (when p
         (toggle-pause))))))
  
(define (preview file-index)
  (with-paused-playback
   (let ([file (schemep3-database-index->filename file-index)])
     (when (file-exists? file)
       (let ([preview-playback-thread
              (thread (lambda () (send (preview-backend) play file void)))])
         (sleep (preview-duration))
         (send (preview-backend) stop)
         (thread-wait preview-playback-thread))))))

(define (paused?)
  (eq? playback-state 'paused))

(define (stop)
  (set! _continue-playback-loop #f)
  (and-let* ((t playback-thread))
    (send (current-backend) stop)
    (thread-wait t))
  (set! _current-track-playlist-index #f))

(define (next)
  (when playback-thread
    (send (current-backend) stop))
  (void))

(define (play)
  (cond
    [(paused?)
     (toggle-pause)]
    [else
     (when (playing?)
       (stop))
     (set! _continue-playback-loop #t)
     (thread-set! playback-thread playback-loop)]))

(define (toggle-pause)
  (cond 
    [(paused?) 
     (set-playback-state 'playing)
     (send (current-backend) pause)]
    [(playing?)
     (set-playback-state 'paused)
     (send (current-backend) pause)]))

(define (seek position)
  (send (current-backend) seek position))

(define (playback-supported? filename)
  (send (current-backend) playback-supported? filename))

(define (playing?)
  (eq? playback-state 'playing))

(define (now-playing-database-index)
  (and (or (paused?) (playing?)) _current-track-database-index))

(define (now-playing-playlist-index)
  (and (or (paused?) (playing?)) _current-track-playlist-index))

;;; run time code...
(playlist-add-hook
 (match-lambda*
   ((list 'remove index playlist)
    (when (and
           (eq? playlist (playlists-playing))
           _current-track-playlist-index
           (<= index _current-track-playlist-index))
      (set! _current-track-playlist-index (sub1 _current-track-playlist-index))))
   ((list 'set playlist)
    (when (eq? playlist (playlists-playing))
      (set! _current-track-playlist-index #f)))
   (_ (void))))

;;;;;;;;;;
(preferences:add-panel 
 "Playback"
 (lambda (parent)
   (let* ((v-panel (new vertical-panel%
                        (parent parent)))
          (duration-slider
           (new (stored-value-mixin slider%)
                [label "Preview Duration"]
                [min-value 0]
                [max-value 120]
                [settor-gettor preview-duration]
                [parent v-panel])))
     v-panel)))
