#lang scheme

(provide taglib-extended-tags)

(require srfi/2)
(require srfi/26)
(require (planet untyped/unlib/list))

(require "taglib.ss")
(require "taglib_more")

(define (parse-txxx-frame raw-tag-value)
  (and-let* ((m (regexp-match             
                 (regexp "\\[(.*)\\] (.*) ([^ ]*)")
                 raw-tag-value)))
    (cons 
     (string-upcase (second m))
     (fourth m))))

(define tag-alist-map
  `((#"TRCK" . "TRACKNUMBER")
    (#"TPE1" . "ARTIST")
    (#"TIT2" . "TITLE")
    (#"TALB" . "ALBUM")))

(define (translate-tag raw-tag)
  (let ((tag-type (car raw-tag))
        (tag-value (cdr raw-tag)))
    (or 
      (and (equal? tag-type #"TXXX")
           (parse-txxx-frame tag-value))
      (and-let* ((key (assoc-value/default tag-type tag-alist-map #f)))
        (cons key tag-value))
      raw-tag)))

(define (id3v2-frames filename)
  (and-let* ((raw-tags (taglib-id3v2-frames filename))
             ((not (null? raw-tags))))
    (map translate-tag raw-tags)))

(define (taglib-extended-tags filename)
  (or      
   (id3v2-frames filename)
   (taglib-flac-frames filename)
   (taglib-tags filename)))
          
;(printf "~%-----------------~%")
;(write (taglib-extended-tags "etc/test2.mp3"))
;
;(printf "~%~%~%-----------------~%")
;(write (taglib-extended-tags "etc/test2.flac"))
