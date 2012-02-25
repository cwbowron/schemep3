#lang scheme

(provide 
 playback-backend<%>
 current-backend 
 preview-backend
 )

(define playback-backend<%> 
  (interface ()
    play
    stop
    pause
    seek
    playback-supported?
    ))

(define (backend-guard n)
  (and (is-a? n playback-backend<%>) n))

(define current-backend
  (make-parameter #f))

(define preview-backend
  (make-parameter #f))
