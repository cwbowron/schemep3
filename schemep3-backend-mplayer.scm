#lang scheme

(require scheme/system)
(require scheme/async-channel)
(require srfi/2)
(require srfi/26)

(require "schemep3-status.scm")
(require "schemep3-database.scm")
(require "schemep3-helpers.scm")
(require "schemep3-playback.scm")

(define mplayer-variables (make-hash))

(define (get-mplayer-variable key)
  (hash-ref mplayer-variables key))
            
(define (set-mplayer-variable key value)
  (hash-set! mplayer-variables key value))

(define-syntax define-mplayer-binding
  (syntax-rules ()
    ((_ variable-name)
     (set-mplayer-variable (quote variable-name) #f))))

(define-mplayer-binding ANS_TIME_POSITION)
(define-mplayer-binding ANS_PERCENT_POSITION)

(define MPLAYER-POS-PERCENT #"pausing_keep get_percent_pos\n")
(define MPLAYER-POS-ABS     #"pausing_keep get_time_pos\n")
(define MPLAYER-QUIT        #"pausing_keep quit\n")
(define MPLAYER-PAUSE       #"pausing_toggle get_time_pos\n")

(define _mplayer-executable
  (if (equal? (system-type) 'windows)
      "bin\\mplayer.exe"
      "/opt/local/bin/mplayer"))

(define (start-playback file)
  (let ((command-line
         (format "~A -slave -quiet \"~A\"" _mplayer-executable file)))
    (process command-line)))

(define mplayer-backend% 
  (class* object% (playback-backend<%>) 
    
    (define PLAYBACK-REGEX 
      (filename-extensions-regex (list "mp3" "flac" "m4a" "ogg" "wma")))

    (define/public (playback-supported? filename)
      (regexp-match? PLAYBACK-REGEX filename))
    
    (define mailbox (make-async-channel 5))

    (define (post-message message)
      (async-channel-put mailbox message))

    (define _player-process-info #f)
    
    (define (current-player-stdin)  
      (list-ref _player-process-info 1))
    
    (define (current-player-stderr)  
      (list-ref _player-process-info 3))
    
    (define (current-player-stdout)  
      (list-ref _player-process-info 0))
    
    ;;;; mplayer communication...
    (define (send-mplayer-command bytes)
      (when _player-process-info
        (let ((player-stdin (current-player-stdin)))
          (write-bytes bytes player-stdin)
          (flush-output player-stdin))))
    
    (define (process-mplayer-string str progress-callback)
      (for ((key (in-hash-keys mplayer-variables)))
        (and-let* ((value
                    (or
                     (and-let* ((m (regexp-match (format "~A='([^\n\r]*)'" key) str)))
                       (second m))
                     (and-let* ((m (regexp-match (format "~A=([^\n\r]*)" key) str)))
                       (string->number (second m))))))
          (unless (equal? (get-mplayer-variable key) value)
            (set-mplayer-variable key value)
            (when (eq? key 'ANS_TIME_POSITION)
              (progress-callback 
               (get-mplayer-variable 'ANS_TIME_POSITION)
               (get-mplayer-variable 'ANS_PERCENT_POSITION)))))))
    
    (define (parse-mplayer-output port callback)
      (let ((recurse #t))
        (let ((evt (sync/timeout 1 port mailbox)))
          (cond
            ((eq? evt port)
             (let ((s (read-line port)))
               (cond 
                 ((string? s)              
                  (process-mplayer-string s callback))
                 ((eof-object? s)
                  (set! recurse #f)))))
            ((bytes? evt)
             (send-mplayer-command evt))
            ((not evt)
             (unless (paused?)
               (send-mplayer-command MPLAYER-POS-PERCENT)
               (send-mplayer-command MPLAYER-POS-ABS))
             (void))
            (else (raise "WTF?!"))))
        (when recurse
          (parse-mplayer-output port callback))))
    
    (define/public (play file callback)
      (status:update "Play ~a" file)
      (set! _player-process-info (start-playback file))          
      (parse-mplayer-output (current-player-stdout) callback)
      (close-input-port (current-player-stdout))
      (close-input-port (current-player-stderr))
      (close-output-port (current-player-stdin))
      (values (get-mplayer-variable 'ANS_TIME_POSITION)
              (get-mplayer-variable 'ANS_PERCENT_POSITION)))

    (define/public (stop)
      (post-message MPLAYER-QUIT))
    
    (define/public (pause)
      (post-message MPLAYER-PAUSE))
    
    (define/public (seek position)
      (post-message (string->bytes/utf-8
                     (format "seek ~a 1\n" position))))
    
    (super-new)))

(current-backend (new mplayer-backend%))
(preview-backend (new mplayer-backend%))