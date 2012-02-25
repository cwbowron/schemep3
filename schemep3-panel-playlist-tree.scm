#lang scheme/gui

(provide/contract
 [build-database-tree (-> (listof string?) string? list?)]
 )

(provide tree-view%)

(require mrlib/hierlist)
(require srfi/26)

(require "schemep3-database.scm")
(require "schemep3-playlist.scm")
(require "schemep3-playback.scm")

(define (tree-add-child tree label tree-or-file-index)
  (append tree (list (cons label tree-or-file-index))))

(define (tree-add-item tree node-labels file-index)
  (let ((item-label (car node-labels))
        (labels-rest (cdr node-labels)))
    (cond
      ;;; one of the values did not exist, ignore this item...
      ((not item-label) tree)
      ;;; we've reached the end of the item components list
      ((null? labels-rest)
       (tree-add-child tree item-label file-index))
      ;;; if the tree already has a node with the correct label
      ;;; swap out that node with a node created by adding this item...
      ((assoc item-label tree)
       (for/list ((child tree))
         (if (string=? (car child) item-label)
             (cons item-label
                   (tree-add-item (cdr child) labels-rest file-index))
             child)))
      ;;; add the whole subtree for the new item... 
      (else
       (let ((sub-tree (tree-add-item (list) labels-rest file-index)))
         (tree-add-child tree item-label sub-tree))))))

(define (build-database-tree-from-results index-vectors fields)
  (for/fold ((tree (list)))
    ((index-vector index-vectors))
    (let ((file-index (vector-ref index-vector 0)))
      (tree-add-item
       tree 
       (map
        (lambda (database-field)
          (schemep3-database-retrieve-field file-index database-field))
        fields)
       file-index))))

(define (build-database-tree fields where)
  (let ((r (schemep3-database-select "file_index" where)))
    (build-database-tree-from-results r fields)))

;;; gui
(define folder-mixin
  (mixin (hierarchical-list-compound-item<%>) 
    (hierarchical-list-compound-item<%>)
    (public contents)
    (define (contents)
      (append-map (cut send <> contents) (send this get-items)))
    (super-instantiate())))

(define node-mixin
  (mixin (hierarchical-list-item<%>) 
    (hierarchical-list-item<%>)
    (public contents)
    (define (contents)
      (list (send this user-data)))
    (super-instantiate())))

(define tree-view% 
  (class hierarchical-list%    
    
    (init-field (parent #f))
    (init-field (tree #f))
    
    (define/override (on-double-select item)
      (let ((z (send item contents)))
        (playlist-clear)
        (for ((item z))
          (playlist-add item))
        (play)))
    
    (define/public (build-tree parent t)
      (let ((node-label (car t))
            (node-contents (cdr t)))
        (cond
          ((list? node-contents)
           (let ((folder
                  (send parent new-list folder-mixin)))
             (send (send folder get-editor) insert node-label)
             (for ((child node-contents))
               (build-tree folder child))))
          (else
           (let ((leaf
                  (send parent new-item node-mixin)))
             (send (send leaf get-editor) insert node-label)
             (send leaf user-data node-contents))))))
    
    (super-make-object parent)
    
    (when tree
      (build-tree this tree))))

;(define (playlist-tree)
;  (let ((z (build-database-tree (list "artist" "album" "title") 
;                                "artist LIKE \"%beatles%\"")))
;    (let ((f (make-object frame% "Playlist Tree" #f 400 400)))
;      (let ((hl (new tree-view%
;                     (parent f)
;                     (tree
;                      (cons "Tom Petty" z)))))
;        (send f show #t)))))
;
;(playlist-tree)
