#lang scheme

(provide/contract
 [scrobble:submit
  (->*
   (#:artist string?
             #:album string?           
             #:title string?
             #:duration fixnum?
             #:username string?
             #:password string?)
   (#:love boolean?
           #:ban boolean?
           #:skip boolean?)
   boolean?)]
 )


(provide
 scrobble:now-playing
 )

(require scheme/date)
(require srfi/2)
(require net/url)
(require net/uri-codec)
(require file/md5)
(require (planet untyped/unlib/debug))

;;; See http://www.last.fm/api/submissions for more information about scrobble api

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(debug-enabled? #f)

(define scrobble-client-id "sp3")
(define scrobble-client-version "0.1")

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (seconds->date (current-seconds)) #t)))

(define-syntax with-scrobble-exception-handler
  (syntax-rules ()
    ((_ a ...)
     (with-handlers
         ((exn:fail?
           (lambda (exn) 
             (fprintf (current-error-port)
                      (format "[~A] Scrobble Exception: ~A~%" 
                              (timestamp)
                              exn))
             #f)))
       a ...))))
     
(define-struct authentication
  (session-id now-playing-url submission-url))

(define (read-authentication port)
  ;;; let* for guaranteed sequence
  (let* ((session-id-string (read-line port))
         (now-playing-string (read-line port))
         (submission-string (read-line port)))
    (let ((submission-url (string->url submission-string))
          (now-playing-url (string->url now-playing-string)))
      (make-authentication session-id-string now-playing-url submission-url))))
          
(define (local-read-line port)
  (debug "read-line" (read-line port)))
    
(define (parse-scrobbler-handshake port)
  (let ((z (local-read-line port)))
    (cond 
      ((eof-object? z) #f)
      ((equal? z "OK") 
       (begin0
         (read-authentication port)
         (close-input-port port)))
      (else
       (parse-scrobbler-handshake port)))))

(define (parse-scrobbler-submission port)
  (let ((z (local-read-line port)))
    (cond 
      ((eof-object? z) #f)
      ((equal? z "OK") 
       (close-input-port port)
       #t)
      (else
       (parse-scrobbler-submission port)))))

(define (handshake-url username password)
  (let* ((t (current-seconds))
         (ts (number->string t))
         (m (md5 (bytes-append
                  (md5 (string->bytes/locale password))
                  (string->bytes/locale ts)))))
    (make-url "http"
              #f
              "post.audioscrobbler.com"
              80
              #t
              (list 
               (make-path/param "" (list)))
              (list
               (cons 'hs "true")
               (cons 'p "1.2.1")
               (cons 'c scrobble-client-id)
               (cons 'v scrobble-client-version)
               (cons 'u username)
               (cons 't ts)
               (cons 'a (bytes->string/utf-8 m)))
              #f)))

;(define get-auth-code
;  (let ([auth-code #f])
;    (lambda (user password force?)
;      (when (or force? (not auth-code))
;        (printf "Audioscrobbler: Authenticating~%")
;        (set! auth-code
;              (parse-scrobbler-handshake
;               (get-impure-port
;                (handshake-url user password)))))
;      auth-code)))

(define cached-auth-code #f)

(define (get-auth-code (user #f) (password #f))
  (when (and user password)
    ;;;(printf "Audioscrobbler: Authenticating~%")
    (set! cached-auth-code
          (parse-scrobbler-handshake
           (get-impure-port
            (handshake-url user password)))))
  cached-auth-code)

(define (scrobble:now-playing 
         #:artist artist
         #:album album
         #:title title
         #:duration duration
         #:username username 
         #:password password)
  
  (define (now-playing-params session-id)
    (alist->form-urlencoded 
     `((s . ,session-id)       
       (a . ,artist)
       (t . ,title)
       (b . ,album)
       (l . ,(number->string duration))
       (n . "")
       (m . ""))))

  (define (submit-now-playing auth)
    (let ((session-id (authentication-session-id auth)))
      (let ((now-playing-url (authentication-now-playing-url auth))
            (post-data (now-playing-params session-id)))
        (let ((bytes (string->bytes/utf-8 post-data)))
          (parse-scrobbler-submission
           (post-impure-port
            now-playing-url
            bytes     
            (list 
             "From: schemep3@bowron.us"
             "User-Agent: schemep3"
             "Content-Type: application/x-www-form-urlencoded")))))))
  
  (with-scrobble-exception-handler    
   (or 
    (and-let* ([auth-code (get-auth-code)])
      (submit-now-playing auth-code))
    (and-let* ((auth-code (get-auth-code username password)))
      (submit-now-playing auth-code)))))

(define (scrobble:submit
         #:artist artist
         #:album album
         #:title title
         #:duration duration
         #:username username 
         #:password password
         #:love (love #f)
         #:ban (ban #f)
         #:skip (skip #f)
         )

  (debug "artist" artist)
  (debug "album" album)
  (debug "title" title)
  (debug "duration" duration)
  (debug "username" username)
  (debug "password" password)
    
  (let ((rating 
         (cond 
           (love "L")
           (ban  "B")
           (skip "S")
           (else ""))))
    
    (define (audioscrobbler-params session-id)
      (alist->form-urlencoded 
       `((s . ,session-id)       
         (|a[0]| . ,artist)
         (|t[0]| . ,title)
         (|i[0]| . ,(number->string (current-seconds)))
         (|o[0]| . "P")
         (|r[0]| . ,rating)
         (|l[0]| . ,(number->string duration))
         (|b[0]| . ,album)
         (|n[0]| . "")
         (|m[0]| . ""))))
       
    (define (submit-with-auth-code auth)
      (let ((session-id (authentication-session-id auth)))
        (let ((submission-url (authentication-submission-url auth))
              (post-data (audioscrobbler-params session-id)))
          (let ((bytes (string->bytes/utf-8 post-data)))
            (parse-scrobbler-submission
             (post-impure-port
              submission-url
              bytes     
              (list 
               "From: schemep3@bowron.us"
               "User-Agent: schemep3"
               "Content-Type: application/x-www-form-urlencoded")))))))
  
    (with-scrobble-exception-handler    
     (or 
      (and-let* ((auth-code (get-auth-code)))
        (submit-with-auth-code auth-code))
      (and-let* ((auth-code (get-auth-code username password)))
        (submit-with-auth-code auth-code))))))

;(define (test pw)
;  (scrobble:submit #:artist "John Lennon" #:title "Zippity Zap Zip" #:album "Imagine" #:duration 300 
;                   #:username "cwbowron" #:password pw))
