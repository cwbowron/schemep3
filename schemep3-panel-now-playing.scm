#lang scheme/gui

(provide now-playing-panel%)

;;; local modules
(require srfi/2)
(require srfi/26)

(require "schemep3-database.scm")
(require "schemep3-playback.scm")	
(require "schemep3-helpers.scm")
(require "schemep3-gui-helpers.scm")
(require "schemep3-mixins-gui.scm")
(require "schemep3-file-mover.scm")
(require "taglib.ss")
(require "taglib-extended.scm")
(require "schemep3-context-menu.scm")

(define now-playing-panel%
  (class (checkable-panel-mixin list-box% "Now Playing")

    (define REGEX-FLAC (filename-extensions-regex (list "flac")))
    
    (define (codec-info path)
      (cond [(regexp-match? REGEX-FLAC path)
             "FLAC - Lossless"]
            [else
             (let ([ext (filename-extension path)]
                   [bitrate (taglib-bitrate path)])
               (format "~A - ~A"
                       (string-upcase (bytes->string/utf-8 ext))
                       bitrate))]))
    
    (define current-file #f)
    
    ;;; the double click function for a line is stored in the data of the list item... 
    (define (display-info file-index)
      (set! current-file file-index)
      (send this clear)
      (let ((r (schemep3-database-select 
                "artist, album, title, rating"
                "file_index=?" file-index))
            [path (schemep3-database-index->filename file-index)])
        (for ((result (car r)))
          (send this append (format "~A" result)))
        (let ([p (path->string (path-only path))])
          (send this append p (cut open-folder p)))
        (send this append (path->string (file-name-from-path path)))
        (send this append (codec-info path))
        (send this append "")
        (if (mini-library:member? path)
            (send this append "Mini Libray: Yes")
            (send this append "Mini Libray: No"))
        (and-let* ([extended-tags (taglib-extended-tags path)])
          (and-let* ([asin (find-tag extended-tags "ASIN")])
            (send this append (format "ASIN: ~A" asin) (cut show-ASIN asin)))
          (and-let* ((brainz-album-id (find-tag extended-tags "MUSICBRAINZ ALBUM ID")))
            (send this append (format "MusicBrainz Album: ~A" brainz-album-id))))))

    (define (local-callback control event)
      (cond
        [(and current-file
              (eq? (send event get-event-type) 'list-box-dclick))
         (and-let* ([n (send control get-selection)]
                    [double-click-function (send control get-data n)])
           (double-click-function))]))
    
    (super-new
     (label #f)
     (callback local-callback)
     (font (get-large-mono-font))
     (choices (list "<Not Playing>")))

    (add-pre-play-hook
     (lambda (playlist-index file-index)
       (display-info file-index)))
    
    (schemep3-database-add-update-hook
     (lambda (file-index)
       (when (eq? file-index (now-playing-database-index))
         (display-info file-index))))))
