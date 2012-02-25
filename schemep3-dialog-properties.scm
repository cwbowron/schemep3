#lang scheme/gui

(provide properties-frame%)

(require srfi/2)
(require srfi/54)

(require "schemep3-helpers.scm")
(require "schemep3-gui-helpers.scm")
(require "schemep3-database.scm")
(require "taglib.ss")
(require "taglib-extended.scm")
(require "in-alist.ss")
(require "schemep3-file-mover.scm")

(define (map-> the-list . fn-list)
  (if (null? fn-list)
      the-list
      (apply map-> (map (car fn-list) the-list) (cdr fn-list))))

(define (string-or-bytes-length string-or-bytes)
  (if (bytes? string-or-bytes) 
      (bytes-length string-or-bytes)
      (string-length string-or-bytes)))

(define (formatn n . r)
  (clip-string (apply format r) n))

(define properties-list-box%
  (class list-box%

    (define/private (append-tags tags)
      (let ((pad (apply max (map-> tags car string-or-bytes-length))))
        (for (((key value) (in-alist tags)))
          (send this append (formatn 200 " ~A: ~S" (cat key pad) value)))))

    (define/private (append-category header tags)
      (send this append header)
      (when tags
        (append-tags tags)))    
    
    (define/public (set-file file-index)
      (let ([filename (schemep3-database-index->filename file-index)])
        (send this clear)
        (send this append (path->string (file-name-from-path filename)))
        (send this append (path->string (path-only filename)))
        (cond [(not (file-exists? filename))
               (send this append "File Does not Exist")]
              [else
               (send this append 
                     (if (mini-library:member? filename)
                         "Mini Library: Yes"
                         "Mini Library: No"))
               (append-category "Technical Information:" (taglib-properties filename))
               (append-category "Base Tags:" (taglib-tags filename))
               (append-category "Extended Tags:" (taglib-extended-tags filename))])))
    
    (super-new 
     (font (get-medium-mono-font))     
     (label #f)
     (choices (list)))))

(define properties-frame%
  (class frame%    
    (init-field items)    
    
    (super-new
     (label "Properties")
     (height 500) 
     (width 350))
    
    (define list-box-parent            
      (if (= (length items) 1)
          this
          (new tab-panel%
               (parent this)
               (callback
                (lambda (tab-panel event)
                  (send 
                   list-box set-file
                   (list-ref items (send tab-panel get-selection)))))
               (choices 
                (map-> items schemep3-database-index->filename file-name-from-path path->string)))))
    
    (define list-box (new properties-list-box% 
                          (parent list-box-parent)))
    
    (send list-box set-file (car items))
    
    (new button%
         (label "Close")
         (callback
          (lambda (button event)
            (send this show #f)))
         (parent this))
    
    (send this show #t)))
