#lang scheme/gui

(provide 
 confirm-and-move-files
 ;;;copy-to-mini-library
 mp3-disc:create
 mini-library:copy
 mini-library:target
 mini-library:member?
 archos:copy
 )

(require framework/gui-utils)
(require srfi/2)
(require srfi/26)
(require srfi/54)
(require (planet untyped/unlib/list))

(require "in-alist.ss")
(require "taglib.ss")
(require "schemep3-playback.scm")
(require "schemep3-helpers.scm")
(require "schemep3-status.scm")
(require "schemep3-database.scm")
(require "schemep3-converter.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define AUX-FILE-REGEX
  (filename-extensions-regex (list "cue" "jpg")))

(define (aux-file? file)
  (regexp-match? AUX-FILE-REGEX (path->string file)))

(define (aux-file-list source-folder)
  (for/list (((file type) (in-path source-folder))
             #:when (and (eq? type 'file) (aux-file? file)))
    file))

(define (move-file source destination)
  (cond
    ((file-exists? destination)
     (printf "File already exists: ~A~%" destination)
     #f)
    (else
     (printf "~A -> ~A~%" source destination)
     ;;;(copy-file source destination)
     ;;;(delete-file source)
     (rename-file-or-directory source destination)
     #t)))

(define (move-aux-files source-folder destination-folder)
  (for ((file (aux-file-list source-folder)))
    (let ((dest-file (build-path destination-folder (file-name-from-path file))))
      (move-file file dest-file))))

(define (move-media-file source destination)
  (when (let ((destination-path (path-only destination)))
          (unless (directory-exists? destination-path)
            (printf "Making Directory ~A~%" destination-path)
            (make-directory* destination-path))
          (move-file source destination))
    (and-let* ((db-index (schemep3-database-filename->index source)))
      (let ([canon-destination (media-canonical-path destination)])
        ;;; if for some reason the destination file is already in the database
        ;;; remove it...
        (and-let* ((zip-zap (schemep3-database-filename->index canon-destination #f)))
          (printf "Removing Previous Entry: [~A] ~A~%" zip-zap canon-destination)
          (schemep3-database-remove zip-zap))
        (schemep3-database-update-field db-index "filename" canon-destination)))))

(define (remove-directory-if-empty path)
  (when (empty? (directory-list path))
    (printf "Removing Empty Directory: ~A~%" path)
    (delete-directory path)))

(define (move-files-and-aux source-files destination-files)
  (let ((len (length source-files))
        (previous-directory #f)
        (source-path #f))
    (progress-bar:set-status "Moving files")
    (for/progress ((n len)
                   (source source-files)
                   (destination destination-files))
      (move-media-file source destination)
      (set! source-path (path-only source))
      (unless (equal? previous-directory source-path)
        (move-aux-files source-path (path-only destination))
        (when previous-directory 
          (remove-directory-if-empty previous-directory))
        (set! previous-directory source-path)))
    (remove-directory-if-empty source-path)))

;;; copy mp3 or convert source into mp3 then copy...
(define MP3-FILE-REGEX
  (filename-extensions-regex (list "mp3")))

(define (mp3-convert/copy source destination)
  (cond [(regexp-match? MP3-FILE-REGEX source)
         (if (file-exists? destination)
             (printf "File Already Exists: ~A~%" destination)
             (let ((destination-path (path-only destination)))
               (unless (directory-exists? destination-path)
                 (printf "Making Directory ~A~%" destination-path)
                 (make-directory* destination-path))
               (copy-file source destination)))]
        [else
         (convert-audio-file source destination 'mp3)]))

(define (mp3-convert/copy-files source-file-list destination-file-list)
  (progress-bar:show)
  (progress-bar:set 0)
  (for ((source source-file-list)
        (destination destination-file-list)
        (n (in-naturals 1)))
    (mp3-convert/copy source destination)
    (progress-bar:set (number->integer (* 100 (/ n (length source-file-list))))))
  (progress-bar:hide))
  
(define file-mover-dialog%  
  (class dialog%
    (init-field filelist)
    (init-field target-function)
    (init-field (convert #f))
    (super-new
     (width 640)
     (height 480)
     (label "Destinations"))
    
    (define/public (get-source n)
      (car (send preview-list get-data n)))
    
    (define/public (get-destination n)
      (cdr (send preview-list get-data n)))
    
    (define/public (set-destination n value)
      (send preview-list set-data n (cons (get-source n) value)))
    
    (define (source-file-list)
      (for/list ((n (in-range (send preview-list get-number))))
        (get-source n)))
    
    (define (destination-file-list)
      (for/list ((n (in-range (send preview-list get-number))))
        (get-destination n)))
    
    (define/public (edit-item n)
      (and-let* ((new-string
                  (get-text-from-user 
                   "New Destination" 
                   (format "~A =>" (get-source n))
                   this 
                   (get-destination n))))
        (set-destination n new-string)))
    
    (define preview-list
      (new list-box%
           (parent this)
           (callback 
            (lambda (list-box event)
              (case (send event get-event-type)
                ((list-box-dclick)
                 (and-let* ((index (send list-box get-selection)))
                   (send this edit-item index))))))
           (choices 
            (list))
           (label #f)))
    
    (define (ok-callback . x)      
      (send this show #f)
      (thread 
       (lambda ()
         (cond 
           [convert
            (mp3-convert/copy-files (source-file-list) (destination-file-list))]        
           [else 
            (move-files-and-aux (source-file-list) (destination-file-list))]))))
    
    (define (cancel-callback . x)
      (send this show #f))
    
    (let ((h-panel
           (new horizontal-panel%
                (stretchable-height #f)
                (alignment '(center center))
                (parent this))))          
      (let-values ([(ok-button cancel-button)
                    (gui-utils:ok/cancel-buttons h-panel ok-callback cancel-callback)])
        (for ((file filelist))
          (let ([target (target-function file)])
            (send preview-list append 
                  (gui-utils:trim-string target 200)
                  (cons file target))))))))

(define (remove-articles string)
  (regexp-replace
   ", ([Tt]he|[Aa])$"
   (regexp-replace "^(([Tt]he)|([Aa])) " string "")
   ""))

;;(define (get-artist tags)
;;;  (remove-articles
;;;   (get-tag "ARTIST" tags)))

(define (music-library-path)
  "E:/Music/Library"
  ;;;"C:/tmp"
  )

(define (replace-invalid-path-component-characters str)
  (regexp-replace*
   "\\.\\.\\."
   (regexp-replace*
    "[/\\]"
    str
    "_")
   "___"))

(define (replace-invalid-directory-characters string)
  (regexp-replace*
   ;;; colon can only occur as the second character (after a drive letter)
   "(..):"
   (regexp-replace*
    ;; these cannot occur anywhere in the string
    "[<>*?|\"]"
    string
    "_")
   "\\1-"))

(define (latin-1-ify string)
  (bytes->string/latin-1 (string->bytes/latin-1 string (bytes-ref #"-" 0))))

(define (build-safe-path . path-list)
  (latin-1-ify
   (replace-invalid-directory-characters
    (path->string
     (apply build-path
            (car path-list)
            (map replace-invalid-path-component-characters (cdr path-list)))))))

(define (taglib-tag-retriever index)
  (lambda (tag)
    (car/default
     (taglib-simple-tags
      (schemep3-database-index->filename index)
      tag)
     #f)))

(define (database-tag-retriever index)
  (lambda (tag)
    (schemep3-database-retrieve-field index tag)))

(define (library-target path)
  (let ((path-string path))
    (let ((tags (taglib-simple-tags path-string 'artist 'album 'title 'date 'tracknumber)))
      (let ((artist (remove-articles (list-ref tags 0)))
            (album (list-ref tags 1))
            (title (list-ref tags 2))
            (date  (list-ref tags 3))
            (track (list-ref tags 4)))
        (build-safe-path        
         (music-library-path)
         ;;; B
         (let ([ch (string-ref artist 0)])
           (case ch
             [(#\0 #\1 #\2 #\3 #\4
               #\5 #\6 #\7 #\8 #\9) "0-9"]
             [else (string ch)]))
         ;;; Bob Dylan
         artist
         ;;; 1967 - Greatest Hits
         (if (and date (> date 0))
             (format "~A - ~A"
                     (if (string? date)
                         (substring date 0 4)
                         date)
                     album)
             album)
         ;;; 02 - Blowin' in the Wind.flac
         (format "~A - ~A.~A"
                 (cat track 2 #\0)
                 title
                 (filename-extension path-string)))))))

(define (confirm-and-move-files source-files)
  (let ((dialog 
         (new file-mover-dialog%
              [target-function library-target]
              (filelist 
               (for/list ((file source-files))
                 (if (path? file) 
                     (path->string file)
                     file))))))
    (send dialog show #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mini-library:base-path)
  "E:/Music/minilibrary/Blackberry/Singles"
  ;;;"E:/Music/minilibrary/tmp"
  )

(define (mini-library:target path)
  (let ([index (schemep3-database-filename->index path)])
    (let ((artist (schemep3-database-retrieve-field index 'artist))
          (title (schemep3-database-retrieve-field index 'title)))
      (build-safe-path
       (mini-library:base-path)
       (format "~A - ~A.mp3" 
               (remove-articles artist)
               title)))))

(define (mini-library:copy files)
  (let ([dialog 
         (new file-mover-dialog%
              [convert #t]
              [target-function mini-library:target]
              [filelist (map schemep3-database-index->filename files)])])
    (send dialog show #t)))

(define (mini-library:member? path)
  (file-exists? (mini-library:target path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mp3-disc:target basepath source-file)
  (let ((path-string source-file))
    (let ((tags (taglib-simple-tags path-string 'artist 'album 'title 'date 'tracknumber)))
      (let ((artist (remove-articles (list-ref tags 0)))
            (album (list-ref tags 1))
            (title (list-ref tags 2))
            (date  (list-ref tags 3))
            (track (list-ref tags 4)))
        (build-safe-path
         basepath
         ;;; artist - year - album
         (if (and date (> date 0))
             (format "~A - ~A - ~A" date artist album)
             ;;;(format "~A - ~A - ~A" date artist album)
             (format "~A - ~A" artist album))
         ;;; 02 - Blowin' in the Wind.flac
         (format "~A - ~A.mp3" (cat track 2 #\0) title))))))

(define (mp3-disc:create files)  
  (let* ([basepath
          ;;;(get-directory "Output Directory...")
          "E:/Music/minilibrary/tmp"
          ]
         [dialog 
          (new file-mover-dialog%
               [convert #t]
               [target-function (cut mp3-disc:target basepath <>)]
               [filelist (map schemep3-database-index->filename files)])])
    (send dialog show #t)))

(define (archos:target basepath source-file)
  (let* ((path-string source-file)
         (tags (taglib-simple-tags path-string 'artist 'album 'title 'date 'tracknumber))
         (artist (remove-articles (list-ref tags 0)))
         (album (list-ref tags 1))
         (title (list-ref tags 2))
         (date  (list-ref tags 3))
         (track (list-ref tags 4)))
    (build-safe-path
     basepath
     ;;; artist
     artist    
     ;;; year - album
     (if (and date (> date 0))
         (format "~A - ~A" date album)
         (format "~A" album))
     ;;; 02 - Blowin' in the Wind.flac
     (format "~A - ~A.mp3" (cat track 2 #\0) title))))

(define (archos:copy files)
  (let* ([basepath "E:/Music/minilibrary/tmp"]
         [dialog 
          (new file-mover-dialog%
               [convert #t]
               [target-function (cut archos:target basepath <>)]
               [filelist (map schemep3-database-index->filename files)])])
    (send dialog show #t)))
