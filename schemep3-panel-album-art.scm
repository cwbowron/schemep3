#lang scheme/gui

(provide album-art-panel%)

(require framework/gui-utils)

(require srfi/2)
(require srfi/26)

(require "schemep3-playback.scm")
(require "schemep3-database.scm")
(require "schemep3-mixins-gui.scm")
(require "schemep3-helpers.scm")

(define album-art-panel%
  (class (checkable-panel-mixin message% "Album Art")
    
    (define _default-image "etc/schemep3.png")
    
    (define _current-bitmap #f)
    
    (define _last-width 0)
    (define _last-height 0)
    
    (init-field (image-width 120))
    (init-field (image-height 120))

    (define delayed-refresh
      (let ([cancel-previous #f])
        (lambda ()
          (when _current-bitmap
            (when cancel-previous
              (cancel-previous))
            (set! cancel-previous
                  (gui-utils:delay-action 
                   1
                   (cut set-album-art-bitmap _current-bitmap)
                   (cut set! cancel-previous #f)))))))
    
    (define/override (on-size w h)
      (unless (and (= w _last-width)
                   (= h _last-height))
        (set! _last-width w)
        (set! _last-height h)        
        (delayed-refresh)))
    
    (define (read-bitmap-or-fail file)
      (and-let* (((file-exists? file))
                 (bmp (make-object bitmap% file))
                 ((send bmp ok?)))
        bmp))
    
    (define (default-bitmap)
      (read-bitmap-or-fail _default-image))
    
    (define BITMAP-REGEX   
      (filename-extensions-regex (list "jpg" "gif" "bmp")))
    
    (define (possible-album-art? file)
      (let ([str (path->string file)])
        (and
         (regexp-match? (regexp-quote "front" #f) str)
         (regexp-match? BITMAP-REGEX str))))
    
    (define (find-album-art path)
      (or
       (read-bitmap-or-fail (build-path path "folder.jpg"))
       (for/or ((possible (find-files possible-album-art? path)))
         (read-bitmap-or-fail possible))
       (default-bitmap)))
        
    (define (target-dimensions sx sy)
      (let ((dx (send this get-width))
            (dy (send this get-height)))
        (let* ((scale (/ sy sx)))
          (if (> (* dx scale) dy)
              (values (number->integer (/ dy scale)) dy)
              (values dx (number->integer (* dx scale)))))))
    
    (define (center-offset target-size source-size)
      (number->integer (/ (- target-size source-size) 2)))
    
    (define/public (set-album-art-bitmap source-bitmap)
      (set! _current-bitmap source-bitmap)
      (let ((source-width (send source-bitmap get-width))
            (source-height (send source-bitmap get-height))
            (canvas-width (send this get-width))
            (canvas-height (send this get-height)))
        (let-values ([(scaled-width scaled-height)
                      (target-dimensions source-width source-height)])
          (let* ((target-bitmap (make-object bitmap% canvas-width canvas-height))
                 (dc (new bitmap-dc% (bitmap target-bitmap))))
            (send dc set-background (make-object color% 220 220 220))
            (send dc clear)
            (let ((x (center-offset canvas-width scaled-width))
                  (y (center-offset canvas-height scaled-height)))
              (send dc draw-bitmap-section-smooth source-bitmap
                    x y scaled-width scaled-height
                    0 0 source-width source-height #f))
            (send dc set-bitmap #f)
            (send this set-label target-bitmap)))))
    
    (define/public (set-album-art file)
      (and-let* ((a (find-album-art (path-only file))))
        (set-album-art-bitmap a)))
    
    (super-new
     (stretchable-height #f)
     (stretchable-width #f)
     (label (make-object bitmap% image-width image-height #f)))
    
    (add-pre-play-hook
     (lambda (index file-index)       
       (set-album-art (schemep3-database-index->filename file-index))))))

;(define album-art-drop-target-frame%
;  (class frame% 
;    
;    (define/override (on-drop-file p)
;      (printf "on-drop-file: ~A~%" p))
;    
;    (super-new
;     (min-height 300)
;     (min-width 300)
;     (label "Drop Target"))
;    
;    (send this accept-drop-files #t)))
;
;(define (z)
;  (let ([f (new album-art-drop-target-frame% 
;                [parent #f])])
;    (send f show #t)))
;
;(z)
;
;(define (test)
;  (let ((f (new frame% (parent #f) (label "Test")))
;        (p "E:/Music/library/B/Beatles/Dr. Ebbetts/MFSL/1966 - Revolver (MFSL Ebbetts)/02 - Eleanor Rigby.flac"))
;    (let ((m (new album-art-panel% (parent f) 
;                  (min-height 300)
;                  (min-width 500))))
;      (send f show #t)
;      (send m set-album-art p))))
