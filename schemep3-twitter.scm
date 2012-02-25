#lang scheme/gui

(require framework)

(require srfi/2)
(require net/url)

(require "schemep3-database.scm")
(require "schemep3-playback.scm")
(require "schemep3-helpers.scm")
(require "schemep3-mixins-gui.scm")     
(require "cwb-twitter.scm")
(require "schemep3-frame-console.scm")

(define twitter-username (preferences-backed-variable 'twitter-username ""))
(define twitter-password (preferences-backed-variable 'twitter-password ""))

(define twitter-last-album (preferences-backed-variable 'twitter-last-album #f))
(define twitter-post-albums? (preferences-backed-variable 'twitter-post #t))

(define URL-STRING 
  "http://spreadsheets.google.com/pub?key=pPmLXr3LkwYHyxM2JLR-ILg&single=true&gid=0&range=D2%3AD2&output=txt")

(define (get-album-count)
  (let ([url (string->url URL-STRING)])
    (let ([p (get-pure-port url)])
      (begin0
        (read p)
        (close-input-port p)))))

(define (twitter-post-album file-index)
  (and-let* ((artist (schemep3-database-retrieve-field file-index 'artist))
             (album (schemep3-database-retrieve-field file-index 'album))
             (n (get-album-count)))
    (let ([msg (format "... ~A - ~A - ~A #albums" (add1 n) artist album)])
      (console:printf "[twitter] Twittering: ~A~%" msg)
      (twitter (twitter-username) (twitter-password) msg))))

(define-syntax in-throw-away-thread
  (syntax-rules ()
    ((_ failure-message body ...)
     (thread
      (lambda ()
        (with-handlers
            ((exn:fail?
              (lambda (exn) 
                (printf (format "[twitter] ~A: ~A~%" failure-message exn))
                #f)))
          body ...))))))

(add-pre-play-hook
 (lambda (index item)
   (when (and (twitter-post-albums?) 
              (> (string-length (twitter-username)) 0)
              (> (string-length (twitter-password)) 0))
     (let ([album (schemep3-database-retrieve-field item 'album)])
       (unless (equal? (twitter-last-album) album)
         (twitter-last-album album)
         (in-throw-away-thread
          "Twitter Failed"
          (twitter-post-album item)))))))

(preferences:add-panel 
 "Twitter"
 (lambda (parent)
   (let* ((v-panel (new vertical-panel%
                        (parent parent)))
          (username-field
           (new (stored-value-mixin text-field%)
                (label "Username:")
                (parent v-panel)
                (settor-gettor twitter-username)))
          (password-field
           (new (stored-value-mixin text-field%)
                (parent v-panel)
                (style '(password single))
                (settor-gettor twitter-password)
                (label "Password:")))
          (password-2-field 
           (new text-field%
                (parent v-panel)
                (init-value (twitter-password))
                (style '(password single))
                (label "Reenter Password:")))
          (twitter-check-box
           (new (stored-value-mixin check-box%)
                [parent v-panel]
                [label "Auto post albums"]
                [settor-gettor twitter-post-albums?])))
     (preferences:add-can-close-dialog-callback
      (lambda ()
        (if (string=?
             (send password-field get-value)
             (send password-2-field get-value))
            #t
            (begin
              (message-box "Schemep3 Preferences" 
                           "Password fields do not match"
                           (send v-panel get-top-level-window)
                           '(ok))
              #f))))
     v-panel)))
