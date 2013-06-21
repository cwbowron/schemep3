#lang scheme

(provide vlc-backend%)

(require scheme/async-channel)
(require scheme/date)

(require srfi/2)
(require srfi/26)

(require "with-finalized-binding.ss")
(require "vlc-ffi.ss")
(require "schemep3-status.scm")
(require "schemep3-database.scm")
(require "schemep3-helpers.scm")
(require "schemep3-backend.scm")

;(define get-vlc-exception
;  (let ([vlc-exception (delay (make-vlc-exception #f 0 ""))])
;    (lambda ()
;      (force vlc-exception))))
;
;(define (call-vlc vlc-fn . fn-args)
;  (let ([exception (get-vlc-exception)])
;    (begin0
;      (apply vlc-fn (append fn-args (list exception)))
;      (when (vlc-exception-raised exception)
;        (error "VLC Exception:"
;               (vlc-exception-code exception)
;               (vlc-exception-message exception))))))

(define (call-vlc vlc-fn . fn-args)
  (apply vlc-fn fn-args))
  
(define get-vlc-instance
  (let ([vlc-instance 
         (delay
          (let ([vlc-args
                 (vector
                  "--plugin-path=C:\\Program Files\\VideoLAN\\VLC\\plugins"
                  "-I" "dummy")])
            (call-vlc libvlc_new (vector-length vlc-args) vlc-args)))])
    (lambda ()     
      (force vlc-instance))))

;;; returns false if vlc media player is done...
(define (vlc-is-still-playing? media-player)
  (let ([state (call-vlc libvlc_media_player_get_state media-player)])
    ;;;(printf "~A - vlc-state (playing?): ~A~%" 
    ;;;        (parameterize ([date-display-format 'iso-8601])
    ;;;          (date->string (seconds->date (current-seconds)) #t))
    ;;;        state)
    (not (member state '(libvlc_Stopped libvlc_Ended libvlc_Error)))))

;;; wait for a playing / paused state
(define (vlc-wait-state media-player)
  (let ([state (call-vlc libvlc_media_player_get_state media-player)])
    ;;;(printf "~A - vlc-state (waiting): ~A~%" 
    ;;;        (parameterize ([date-display-format 'iso-8601])
    ;;;          (date->string (seconds->date (current-seconds)) #t))
    ;;;        state)
    (unless (member state '(libvlc_Playing libvlc_Paused libvlc_Error))
      (sleep .1)
      (vlc-wait-state media-player))))

(define (path->mrl path-string)
  (string-append "file://" path-string))        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vlc-backend% 
  (class* object% (playback-backend<%>) 
    
    (define PLAYBACK-REGEX 
      (filename-extensions-regex (list "mp3" "flac" "m4a" "ogg" "wma")))
    
    (define mailbox (make-async-channel 5))

    (define (post-message message)
      (async-channel-put mailbox message))
    
    (define (handle-playback-messages media-player)
      (and-let* ((msg (sync/timeout .5 mailbox)))
        (msg media-player)))
    
    (define (internal-playback-loop media-player callback)
      (let ([time (call-vlc libvlc_media_player_get_time media-player)]
            [position (call-vlc libvlc_media_player_get_position media-player)])
        
        (let ([time (number->integer (/ time 1000.0))]
              [position (number->integer (* 100.0 position))])
          (callback time position)
          
          (handle-playback-messages media-player)
          (if (vlc-is-still-playing? media-player)
              (internal-playback-loop media-player callback)
              (values time position)))))
    
    (define/public (play file callback)
      (with-finalized-binding
       (lambda () (call-vlc libvlc_media_new_path (get-vlc-instance) (path->mrl file)))
       (lambda (media)
         (with-finalized-binding
          (lambda () (call-vlc libvlc_media_player_new_from_media media))
          (lambda (media-player)
            (call-vlc libvlc_media_player_play media-player)
            (vlc-wait-state media-player)
            (internal-playback-loop media-player callback))
          libvlc_media_player_release))
       libvlc_media_release))
    
    (define/public (stop)
      (post-message
       (cut call-vlc libvlc_media_player_stop <>)))
             
    (define/public (pause)
      (post-message
       (cut call-vlc libvlc_media_player_pause <>)))
    
    (define/public (seek position)
      (let ([seek-point (exact->inexact (/ position 100))])
        (post-message
         (cut call-vlc libvlc_media_player_set_position <> seek-point))))
        
    (define/public (playback-supported? filename)
      (regexp-match? PLAYBACK-REGEX filename))
    
    (super-new)))
