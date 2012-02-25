#lang scheme

(define database-index (flat-named-contract "database-index" exact-nonnegative-integer?))

(provide/contract
 ;;; media table
 [media-table-create                 (-> db? void)]
 ;;;[media-table-drop                   (-> db? void)]
 [media-table-select                 (->* (db? string? string?) () #:rest any/c (or/c #f list?))]
 [media-table-select-one             (->* (db? string? string?) () #:rest any/c any/c)]
 [media-table-update-field           (-> db? database-index string? any/c void)]
 [media-table-add-file               (-> db? string? boolean? database-index)]
 [media-table-add-file-temporarily   (-> db? string? database-index)]
 [media-table-add-file-permanently   (-> db? string? database-index)] 
 [media-table-file-index             (-> db? string? (or/c database-index #f))]
 [media-table-update-duration        (-> db? string? database-index void)]
 [media-table-update-metadata        (-> db? string? database-index void)] 
 [media-table-update-last-visit      (-> db? database-index void)]
 [media-table-remove-temporary-files (-> db? void)]
 
 ;;; helpers
 [media-canonical-path               (-> path-string? string?)]
 )

(require srfi/2)
(require scheme/path)
(require scheme/file)
(require scheme/string)

(require (planet jaymccarthy/sqlite/sqlite))
(require (planet untyped/unlib/list))

(require "taglib.ss")
(require "with-finalized-binding.ss")

(define (media-duration filepath)
  (taglib-duration filepath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (media-table-create db)
  (exec/ignore
   db
   #<<SQL
CREATE TABLE IF NOT EXISTS media (
  file_index INTEGER PRIMARY KEY,
  filename  TEXT,
  artist    TEXT,
  album     TEXT,
  title     TEXT,
  rating    INTEGER,
  last_modified INTEGER,
  last_seen INTEGER,
  last_played INTEGER,
  play_count INTEGER,
  duration   INTEGER,
  persist     INTEGER
)
SQL
   ))

;;;(define (media-table-drop db)
;;;  (exec/ignore db "DROP TABLE IF EXISTS media"))

(define (prepare-and-finalize db thunk SQL params)
  (with-finalized-binding 
   (lambda () (prepare db SQL))
   (lambda (statement)
     (unless (null? params)
       (apply load-params statement params))
     (thunk statement))
   finalize))

(define (media-table-select-one db fields where . params)
  (prepare-and-finalize
   db
   (lambda (statement)
     (and-let* ([r (step statement)])
       (vector-ref r 0)))
   (format "SELECT ~A FROM media WHERE ~A" fields where)
   params))

(define (media-table-select db fields where . params)
  (prepare-and-finalize
   db
   step*
   (format "SELECT ~A FROM media WHERE ~A" fields where)
   params))

(define (media-table-new-row db)
  (insert db "INSERT INTO media (last_seen) VALUES (?)" (current-seconds)))

(define (media-table-update-field db file-index field value)
  (exec/ignore db (format "UPDATE MEDIA SET ~A=? WHERE file_index=?" field) value file-index))

(define (media-table-remove-temporary-files db)
  (exec/ignore db "DELETE FROM media WHERE persist=0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (media-table-update-duration db filepath file-index)
  (and-let* ((duration (media-duration filepath)))
    (media-table-update-field db file-index "duration" duration)))
  
(define (media-table-update-metadata db filepath file-index)
  (and-let* ((tags (taglib-simple-tags filepath 'artist 'album 'title)))
    (for ((database-key '("ARTIST" "ALBUM" "TITLE"))
          (tag-value tags))
      (when tag-value
        (media-table-update-field db file-index database-key tag-value)))))
                           
(define (media-table-add-file db filepath temporary?)
  (with-transaction* 
   db 'none 
   (lambda (bail)
     (let ((file-index (media-table-new-row db)))
       (media-table-update-field db file-index "last_modified" (file-or-directory-modify-seconds filepath))
       (media-table-update-field db file-index "persist" (if temporary? 0 1))
       (media-table-update-field db file-index "filename" filepath)
       (media-table-update-duration db filepath file-index)
       (media-table-update-metadata db filepath file-index)
       file-index))))

(define (media-table-add-file-temporarily db filepath)
  (media-table-add-file db filepath #t))

(define (media-table-add-file-permanently db filepath)
  (media-table-add-file db filepath #f))

(define (media-table-file-index db filepath)
  (media-table-select-one db "file_index" "filename=?" filepath))

(define (media-table-update-last-visit db file-index)
  (media-table-update-field db file-index "last_seen" (current-seconds)))

(define (media-canonical-path path)
  (path->string (normal-case-path path)))
