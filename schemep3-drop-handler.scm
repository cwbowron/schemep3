#lang scheme

(provide schemep3-drop-handler:drop)

(require srfi/2)
(require srfi/26)

(require "in-alist.ss")
(require "schemep3-playlist.scm")
(require "schemep3-helpers.scm")
(require "schemep3-status.scm")
(require "schemep3-playback.scm")

(define PLAYLIST-FILE-REGEX
  (filename-extensions-regex (list "m3u")))

(define (playlist-file? filepath)
  (regexp-match? PLAYLIST-FILE-REGEX (if (string? filepath) filepath (path->string filepath))))

(define drop-queue (list))
(define drop-thread #f)
(define drop-queue-semaphore (make-semaphore 1))
(define destination-playlist #f)

(define (drop-playlist filepath)
  (call-with-input-file filepath
    (lambda (in)
      (for ((line (in-lines in)))
        (unless (regexp-match? "^#" line)
          (playlist-add-file line destination-playlist))))))

(define (drop-queue-advance)
  (call-with-semaphore 
   drop-queue-semaphore
   (lambda ()
     (if (empty? drop-queue)
         #f
         (begin0 
           (car drop-queue)
           (set! drop-queue (cdr drop-queue)))))))

(define (drop-queue-append item)
  (call-with-semaphore
   drop-queue-semaphore
   (lambda ()     
     (set! drop-queue (append drop-queue (list item))))))

(define (internal-drop path)
  (cond
    [(playlist-file? path)
     (drop-playlist path)]
    [else
     (let ([n 0])
       (for ((file (in-files path)))
         (let ([str (path->string file)])
           (when (playback-supported? str)
             (set! n (modulo (+ n 10) 100))
             (progress-bar:set n)
             (playlist-add-file str destination-playlist)))))]))

(define (drop-loop)
  (progress-bar:show)
  (let loop-loop ()
    (and-let* ((item (drop-queue-advance)))
      (internal-drop item)
      (loop-loop)))
  (progress-bar:hide))

(define (schemep3-drop-handler:drop path)
  (drop-queue-append path)
  (unless drop-thread
    (set! destination-playlist (playlists-active))
    (thread-set! drop-thread drop-loop)))
