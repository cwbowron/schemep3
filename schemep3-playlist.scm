#lang scheme

;;; contract types and predicates
(define (playlist-index? x) exact-nonnegative-integer?)
(define valid-playlist-index/c (flat-named-contract "valid-playlist-index/c" playlist-index?))

(define index-into-playlists/c (or/c false/c exact-nonnegative-integer?))

(provide valid-playlist-index/c)

(provide/contract
 [playlist-add-hook               (-> procedure? void)]
 
 ;;; calls hook with 'clear playlist-reference
 [playlist-clear                  (->* () 
                                       (index-into-playlists/c) 
                                       void)]
 
 ;;; calls hook with 'add list-of-additional-database-indexs playlist-reference
 [playlist-add                    (->* ((or/c (listof database-index/c) database-index/c)) 
                                       (index-into-playlists/c) 
                                       void)]
 
 [playlist-add-file               (->* ((or/c string? path?))
                                       (index-into-playlists/c)
                                       void)]
 
 ;;; calls hook with 'remove playlist-index playlist-reference
 [playlist-remove                 (->* (valid-playlist-index/c) 
                                       (index-into-playlists/c) 
                                       void)]
 
 [playlist-find-item              (->* (database-index/c)
                                       (index-into-playlists/c)
                                       (listof valid-playlist-index/c))]
 
 [playlist-item                   (->* (valid-playlist-index/c) 
                                       (index-into-playlists/c) 
                                       database-index/c)]
 
 [playlist-item-count             (->* () 
                                       (index-into-playlists/c) 
                                       exact-nonnegative-integer?)]
 
 [playlist-contents               (->* () 
                                       (index-into-playlists/c) 
                                       (listof database-index/c))]
 
 [playlist-set                    (->* ((listof database-index/c))
                                       (index-into-playlists/c)
                                       void)]
 
 [playlist-swap-items             (->* (valid-playlist-index/c valid-playlist-index/c) 
                                       (index-into-playlists/c) 
                                       void)]
                                                                  
 [playlist-set-from-database      (->* (string?) 
                                       () 
                                       #:rest any/c 
                                       any/c)]
 
 ;;; calls hook with 'show-now-playing
 [playlist-show                      (-> valid-playlist-index/c index-into-playlists/c void)]
 
 ;;; selection related
 ;;; calls hook with 'select list-of-new-selections
 [playlist-select                    (-> (or/c valid-playlist-index/c (listof valid-playlist-index/c)) void)]
 [playlist-selected-playlist-indexes (-> (listof valid-playlist-index/c))]
 [playlist-selected-database-indexes (-> (listof database-index/c))]

 ;;; queue related
 ;;; calls hook with 'playback-queue-add
 [playback-queue-clear    (-> void)]
 [playback-queue-add      (->* ((or/c valid-playlist-index/c (listof valid-playlist-index/c)) boolean?) 
                               (index-into-playlists/c)
                               void)]
 
 [playback-queue-member?  (->* (valid-playlist-index/c)
                               (index-into-playlists/c)
                               boolean?)]
  
 [playback-queue-contents (-> list?)]
 ;;;[playback-queue-advance  (-> void)]
 [playback-queue:next!       (-> any/c)]
 
 [playback-queue-member-file-index? (-> database-index/c boolean?)]

 ;;; calls hook with 'playlists-create name
 [playlists-create         (->* () (string?) index-into-playlists/c)]
 [playlists-find           (-> string? index-into-playlists/c)]

 ;;; calls hook with 'playlists-create name [when necessary]
 [playlists-find-or-create (-> string? index-into-playlists/c)]
 
 ;;; calls hook with 'playlists-delete playlist 
 [playlists-delete         (->d ((playlist-index index-into-playlists/c))
                                ()
                                #:pre-cond (> (playlists-count) 1)
                                (_ void))]
 
 [playlists-count          (-> exact-nonnegative-integer?)]
 
 ;;; calls hook with 'playlist-rename new-name playlist
 [playlists-name           (->* (index-into-playlists/c) (string?) string?)]
 ;;; calls hook with 'playlist-set-active
 [playlists-active         (->* () (index-into-playlists/c boolean?) index-into-playlists/c)]
 ;;; calls hook with 'playlist-set-playing
 [playlists-playing        (->* () (index-into-playlists/c boolean?) index-into-playlists/c)]
 )

(require scheme/mpair)

(require srfi/2)
(require srfi/26)

(require (planet untyped/unlib/list))

(require "schemep3-status.scm")
(require "schemep3-helpers.scm")  
(require "schemep3-database.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define _playback-queue (list))
(define _playlist-selected (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define _playlists (list))

(define-unnamed-hook playlist-add-hook call-playlist-hooks)

(define (item-or-list->list x)
  (if (list? x) x (list x)))

;;; playlist operations
(define (playlist-contents (playlist (playlists-active)))
  (mcdr (list-ref _playlists playlist)))

(define (playlist-set-contents new-contents (playlist (playlists-active)))
  (set-mcdr! (list-ref _playlists playlist) new-contents))

(define (playlist-set new-contents (playlist (playlists-active)))
  (playlist-set-contents new-contents playlist)
  (when (= playlist (playlists-active))
    (set! _playlist-selected (list)))
  (call-playlist-hooks 'set playlist))

(define (playlist-clear (playlist (playlists-active)))
  (playlist-set-contents (list) playlist)
  (when (= playlist (playlists-active))
    (set! _playlist-selected (list)))
  (call-playlist-hooks 'clear playlist))

(define (playlist-add addition (p (playlists-active)))
  (let ((addition-list (item-or-list->list addition)))
    (playlist-set-contents (append (playlist-contents p) addition-list) p)
    (call-playlist-hooks 'add addition-list p)))

(define (playlist-add-file filepath (playlist (playlists-active)))
  (playlist-add (schemep3-database-filename->index filepath) playlist))

(define (playlist-remove playlist-index (playlist (playlists-active)))
  (playlist-set-contents 
   (list-remove-item (playlist-contents playlist) playlist-index) 
   playlist)
  (call-playlist-hooks 'remove playlist-index playlist)
  ;;; clean up the selected indexes...
  (let ([selected (playlist-selected-playlist-indexes)])
    (unless (empty? selected)
      (playlist-select
       (for/list ((s-index selected)
                  #:when (not (= s-index playlist-index)))
         (if (< s-index playlist-index)
             s-index
             (- s-index 1)))))))

(define (playlist-item playlist-index (playlist (playlists-active)))
  (list-ref (playlist-contents playlist) playlist-index))

(define (playlist-item-count (playlist (playlists-active)))
  (length (playlist-contents playlist)))

(define (playlist-select items)
  (set! _playlist-selected (item-or-list->list items))
  (call-playlist-hooks 'select _playlist-selected))

(define (playlist-selected-playlist-indexes)
  _playlist-selected)

(define (playlist-selected-database-indexes)
  (map playlist-item (playlist-selected-playlist-indexes)))

(define (playlist-swap-items n m (playlist (playlists-active)))
  (playlist-set-contents (list-swap (playlist-contents playlist) n m) playlist)
  (call-playlist-hooks 'set-item n (playlist-item n playlist) playlist)
  (call-playlist-hooks 'set-item m (playlist-item m playlist) playlist))

(define (playlist-show n p)
  (call-playlist-hooks 'show n p))

;;; playback queue structure...
;;; list of (playlist-index playlist-reference database-index)
(define (playback-queue-contents)
  _playback-queue)

;;;(define (playback-queue-advance)
;;;  (set! _playback-queue (cdr _playback-queue)))

(define (playback-queue:next!)
  (if (null? _playback-queue) 
      #f
      (begin0
        (car _playback-queue)
        (set! _playback-queue (cdr _playback-queue)))))
         

(define (playback-queue-clear)
  (let ([Q _playback-queue])
    (set! _playback-queue (list))
    (call-playlist-hooks 'playback-queue-clear Q)))

(define (make-queue-item item-index playlist)
  (list item-index playlist (playlist-item item-index playlist)))

(define (make-queue-items indexes playlist)
  (map (cut make-queue-item <> playlist) indexes))

(define (playback-queue-member? playlist-index (playlist (playlists-active)))
  (for/or ((q-item (playback-queue-contents)))
    (and (= playlist-index (first q-item))
         (= playlist (second q-item)))))
          
(define (playback-queue-member-file-index? file-index)
  (for/or ((q-item (playback-queue-contents)))
    (= file-index (third q-item))))
         
(define (playback-queue-add addition top? (playlist (playlists-active)))
  (let ((addition-list 
         (make-queue-items (item-or-list->list addition) playlist)))
    (set! _playback-queue
          (if top? 
              (append addition-list _playback-queue)
              (append _playback-queue addition-list)))
    (call-playlist-hooks 'playback-queue-add)))

(define (playlist-set-from-database where-clause . sql-params) 
  (status:update "playlist-add-from-database ~A" where-clause)
  (let ((db-result 
         (apply schemep3-database-select "file_index" where-clause sql-params)))
    (if (not (null? db-result))
        (playlist-set (map (cut vector-ref <> 0) db-result) (playlists-active))
        (status:update "No Matches for ~A" where-clause))))

;;; playlist level operations
(define (playlists-create (name #f))
  (let ((pname (or name "new playlist")))
    (set! _playlists (append _playlists (list (mcons pname (list)))))
    (call-playlist-hooks 'playlists-create pname)
    (- (length _playlists) 1)))

(define (playlists-find name)
  (for/or (((playlist index) (in-indexed _playlists)))
    (and (equal? name (mcar playlist)) index)))

(define (playlists-find-or-create name)
  (or (playlists-find name) 
      (playlists-create name)))

(define (playlists-delete playlist-index)
  (set! _playlists (list-remove-item _playlists playlist-index))
  (let ([n (max 0 (- playlist-index 1))])
    (when (= (playlists-playing) playlist-index)
      (playlists-playing n #t))
    (when (= (playlists-active) playlist-index)
      (playlists-active n #t)))
  (call-playlist-hooks 'playlists-delete playlist-index))
  
(define (playlists-name playlist-index (new-name #f))
  (when (and new-name (not (string=? new-name (playlists-name playlist-index))))
    (set-mcar! (list-ref _playlists playlist-index) new-name)
    (call-playlist-hooks 'playlists-rename new-name playlist-index))
  (mcar (list-ref _playlists playlist-index)))
  
(define (playlists-count)
  (length _playlists))

(define-syntax define-playlist-variable
  (syntax-rules ()
    ((_ accessor hook)
     (define accessor
       (let ((v 0))
         (lambda ((new-value #f) (force #f))
           (when (and new-value 
                     (or force (not (= new-value v))))
             (set! v new-value)
             (call-playlist-hooks hook new-value))
           v))))))
  
(define-playlist-variable playlists-active 'playlists-set-active)
(define-playlist-variable playlists-playing 'playlists-set-playing)

(define (playlist-find-item item (p (playlists-active)))
  (for/list (((entry n) (in-indexed (playlist-contents p)))
             #:when (eq? item entry))
    n))

;;; run time code
(schemep3-database-add-removal-hook
 (lambda (index)   
   (for* ((playlist (in-range (playlists-count)))
          (p-index (sort (playlist-find-item index playlist) >)))
     (playlist-remove p-index playlist))))
