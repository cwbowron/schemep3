#lang scheme/gui

(provide
 file-operations:add
 playlist-operations:add
 
 generate-context-menu
 
 make-file-operation
 make-file-operation-group
 make-file-operation-generator
 )

(require "schemep3-playlist.scm")
(require "schemep3-helpers.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; playlist-ops expect a list of playlist indices
(define context-menu-playlist-ops (list))

;;; file-ops expect a list of file database indices
(define context-menu-file-ops (list))

(define (process-context-list parent op-list file-or-playlist-indexes)
  (for ((item op-list))
    (item parent file-or-playlist-indexes)))

(define (generate-context-menu parent (file-indexes #f))
  (unless file-indexes
    (process-context-list parent context-menu-playlist-ops
                          (playlist-selected-playlist-indexes)))
  (process-context-list parent context-menu-file-ops 
                        (or file-indexes (playlist-selected-database-indexes))))


(define-struct file-operation (label function)
  #:property prop:procedure 
  (lambda (file-op parent-menu file-indexes)
    (new menu-item%
         (label (file-operation-label file-op))
         (parent parent-menu)
         (callback
          (lambda (m e)
            (thread 
             (lambda () 
               ((file-operation-function file-op) file-indexes))))))))

(define-struct file-operation-group (label children)
  #:property prop:procedure   
  (lambda (file-op parent-menu file-indexes)
    (let ((submenu
           (new menu%
                (parent parent-menu)
                (label (file-operation-group-label file-op)))))
      (for ((group-item (file-operation-group-children file-op)))
        (group-item submenu file-indexes)))))
    
(define-struct file-operation-generator (function)
  #:property prop:procedure
  (lambda (file-op parent-menu file-indexes)
    ((file-operation-generator-function file-op) parent-menu file-indexes)))

(define (file-operations:add file-op-or-group-or-generator)
  (push! context-menu-file-ops file-op-or-group-or-generator))

(define (playlist-operations:add label function)
  (push! context-menu-playlist-ops (make-file-operation label function)))
