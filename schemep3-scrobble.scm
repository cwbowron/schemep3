#lang scheme/gui

(require srfi/2)
(require framework)

(require "cwb-scrobble.scm")
(require "schemep3-database.scm")
(require "schemep3-playback.scm")
(require "schemep3-helpers.scm")
(require "schemep3-mixins-gui.scm")
(require "schemep3-frame-console.scm")

(define audioscrobbler-username (preferences-backed-variable 'scrobble-username ""))
(define audioscrobbler-password (preferences-backed-variable 'scrobble-password ""))

(define (scrobble?)
  (and (> (string-length (audioscrobbler-username)) 0)
       (> (string-length (audioscrobbler-password)) 0)))

(add-pre-play-hook
 (lambda (index file-index)
   (and-let* (((scrobble?))
              (filename (schemep3-database-index->filename file-index))
              (artist   (schemep3-database-retrieve-field file-index 'artist))
              (album    (schemep3-database-retrieve-field file-index 'album))
              (title    (schemep3-database-retrieve-field file-index 'title))
              (duration (schemep3-database-retrieve-field file-index 'duration)))
     (thread
      (lambda ()
        (if 
         (scrobble:now-playing
          #:artist artist
          #:album album
          #:title title
          #:duration (inexact->exact duration)
          #:username (audioscrobbler-username)
          #:password (audioscrobbler-password))
         (console:printf "[scrobble] Now Playing Win  - ~A~%" filename)
         (console:printf "[scrobble] Now Playing Fail - ~A~%" filename)))))))

(add-post-play-hook
 (lambda (index file-index elapsed percent)
   (and-let* (((scrobble?))
              (filename (schemep3-database-index->filename file-index))
              (artist   (schemep3-database-retrieve-field file-index 'artist))
              (album    (schemep3-database-retrieve-field file-index 'album))
              (title    (schemep3-database-retrieve-field file-index 'title))
              (duration (schemep3-database-retrieve-field file-index 'duration))
              (rating   (schemep3-database-retrieve-field file-index 'rating))
              ((>= percent 50))
              ((>= elapsed 30)))
     (thread
      (lambda ()
        (if 
         (scrobble:submit
          #:artist artist
          #:album album
          #:title title
          #:love (and rating (> rating 4))
          #:duration (inexact->exact duration)
          #:username (audioscrobbler-username)
          #:password (audioscrobbler-password))
         (console:printf "[scrobble] Scrobble Win  - ~A~%" filename)
         (console:printf "[scrobble] Scrobble Fail - ~A~%" filename)))))))

(preferences:add-panel 
 "AudioScrobbler"
 (lambda (parent)
   (let* ((v-panel (new vertical-panel%
                        (parent parent)))
          (username-field           
           (new (stored-value-mixin text-field%)                
                (parent v-panel)
                (settor-gettor audioscrobbler-username)
                (label "Username:")))
          (password-field
           (new (stored-value-mixin text-field%)
                (parent v-panel)
                (style '(password single))
                (settor-gettor audioscrobbler-password)
                (label "Password:")))
          (password-2-field 
           (new text-field%
                (parent v-panel)
                (init-value (or (audioscrobbler-password) ""))
                (style '(password single))
                (label "Reenter Password:"))))
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
