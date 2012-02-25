#lang scheme

(define database-index/c (flat-named-contract "database-index" exact-positive-integer?))

(define media-table-fields '(filename artist album title duration last_seen rating last_played play_count last_modified))
(define (database-field? x) (or (string? x) (member x media-table-fields)))
(define database-field/c (flat-named-contract "database field" database-field?))
                                              
(provide database-index/c)
(provide database-field/c)

(provide/contract
 [schemep3-database-select              (->* (string? string?) () #:rest any/c any)]
 [schemep3-database-select-one          (->* (string? string?) () #:rest any/c any)]
 [schemep3-database-filename->index     (->* (string?) (any/c) (or/c database-index/c #f))]
 [schemep3-database-retrieve-field      (-> database-index/c database-field/c any/c)]
 [schemep3-database-update-field        (-> database-index/c database-field/c any/c void)]
 [schemep3-database-index->filename     (-> database-index/c string?)]
 [schemep3-database-increase-play-count (-> database-index/c void)]
 [schemep3-database-reload-item         (-> database-index/c boolean? void)]
 [schemep3-database-remove              (-> database-index/c void)]
 [schemep3-database-scan-directory      (-> (or/c path? string?) void)]
 [schemep3-database-add-update-hook     (-> procedure? void)]
 [schemep3-database-add-removal-hook    (-> procedure? void)]
 )

(provide 
 schemep3-database-close
 database
 media-canonical-path)

(require srfi/2)
(require srfi/26)

(require (planet jaymccarthy/sqlite/sqlite))

(require "in-alist.ss")
(require "cwb-mp3-db.scm")  
(require "schemep3-helpers.scm")
(require "schemep3-frame-console.scm")

(define _db #f)
(define _database-filename "etc/schemep3.db")

(define MAX-READ-SEMAPHORE 5)

(define read-semaphore (make-semaphore MAX-READ-SEMAPHORE))
(define lockout-semaphore (make-semaphore 1))

(define (acquire-read-lock)
  (semaphore-wait lockout-semaphore)
  (semaphore-wait read-semaphore)
  (semaphore-post lockout-semaphore))

(define (release-read-lock)
  (semaphore-post read-semaphore))

(define (acquire-write-lock)
  (semaphore-wait lockout-semaphore)
  (for ((n (in-range MAX-READ-SEMAPHORE)))
    (semaphore-wait read-semaphore))
  (semaphore-post lockout-semaphore))

(define (release-write-lock)
  (for ((n (in-range MAX-READ-SEMAPHORE)))
    (semaphore-post read-semaphore)))
  
(define (with-media-database-read f)
  (dynamic-wind
   acquire-read-lock
   (lambda () (f (database)))
   release-read-lock))

(define (with-media-database-write f)
  (dynamic-wind
   acquire-write-lock
   (lambda () (f (database)))
   release-write-lock))

(define-unnamed-hook schemep3-database-add-update-hook  call-update-hooks)
(define-unnamed-hook schemep3-database-add-removal-hook call-removal-hooks)

;;;;
(define (schemep3-database-open)
  (set! _db (open (string->path _database-filename)))
  (media-table-create _db)
  _db)

(define (schemep3-database-close)
  (when _db
    (close _db)
    (set! _db #f)))

(define (database)
  (or _db (schemep3-database-open)))

(define (database-field->string field)
  (if (string? field) field (symbol->string field)))

(define (schemep3-database-select-one fields where . params)
  (with-media-database-read
   (cut apply media-table-select-one <> fields where params)))

(define (schemep3-database-select fields where . params)
  (with-media-database-read 
   (cut apply media-table-select <> fields where params)))

(define (schemep3-database-retrieve-field file-index field)
  (schemep3-database-select-one (database-field->string field) "file_index=?" file-index))

(define (schemep3-database-update-field file-index field value)
  (with-media-database-write 
   (cut media-table-update-field <> file-index (database-field->string field) value))
  (call-update-hooks file-index))

(define (schemep3-database-reload-duration item filename force?)
  (let ((database-duration (schemep3-database-retrieve-field item 'duration)))
    (cond
      [(or force? (not database-duration) (= database-duration 0))
       (with-media-database-write 
        (cut media-table-update-duration <> filename item))
       #t]
      [else #f])))

(define (schemep3-database-reload-metadata item filename force?)
  (let ((timestamp-file
         (file-or-directory-modify-seconds filename))
        (timestamp-database
         (schemep3-database-retrieve-field item 'last_modified)))
    (cond
      [(or force? (> timestamp-file timestamp-database))
       (with-media-database-write
        (cut media-table-update-metadata <> filename item))
       (unless (= timestamp-file timestamp-database)
         (schemep3-database-update-field item 'last_modified timestamp-file))
       #t]
      [else #f])))

(define (schemep3-database-reload-item item force?)
  (let ((filename (schemep3-database-index->filename item)))
    (if (file-exists? filename)
        (when (complete-or
               (schemep3-database-reload-duration item filename force?)
               (schemep3-database-reload-metadata item filename force?))
            (call-update-hooks item))
        (console:printf "[database] File Does Not Exist: ~A~%" filename))))

(define (schemep3-database-remove index)
  (and-let* ((fn (schemep3-database-index->filename index)))
    (console:printf "[database] - ~A~%" fn))
  (with-media-database-write
   (cut exec/ignore <> "DELETE FROM media WHERE file_index=?" index))
  (call-removal-hooks index))

(define (schemep3-database-scan-directory directory)
  (with-media-database-write
   (cut media-table-scan-directory <> directory)))

;;; retrieves the index if it exists, otherwise add it and return the new index
(define (schemep3-database-filename->index filename (add? #t))
  (let ((canonical-path (media-canonical-path filename)))
    (or 
     (with-media-database-read 
      (cut media-table-file-index <> canonical-path))
     (and 
      add?
      (begin0
        (with-media-database-write 
         (cut media-table-add-file-temporarily <> canonical-path))
        (console:printf "[database] + ~A~%" canonical-path))))))

(define (schemep3-database-index->filename index)
  (schemep3-database-retrieve-field index 'filename))

(define (schemep3-database-increase-play-count item)
  (let ((previous-count
         (or 
          (schemep3-database-retrieve-field item 'play_count)
          0)))
    (schemep3-database-update-field item 'play_count (+ 1 previous-count))))

(define MUSIC-FILE-REGEX 
  (filename-extensions-regex (list "mp3" "flac" "ogg")))

(define (music-file? filename)
  (regexp-match? MUSIC-FILE-REGEX  (path-or-string->string filename)))

(define (directory-scanner db path bail-fn)
  (for/fold ([count 0])
            ((file (in-files path))
             #:when (music-file? file))
    (let ((canon-path (media-canonical-path file)))
      (and-let* ((file-index (media-table-file-index db canon-path)))
        (media-table-update-last-visit db file-index)
        (media-table-add-file-permanently db canon-path)))
    (add1 count)))

(define (media-table-scan-directory db base-directory)
  (let ((start-time (current-seconds))
        (filecount 
         (with-transaction* db 'none (cut directory-scanner db base-directory <>))))
    (let ((elapsed (- (current-seconds) start-time)))
      (console:printf
       "[database] Examined ~a files in ~a minutes~%" 
       filecount
       (exact->inexact (/ elapsed 60))))))
