#lang scheme

(provide 
 taglib-properties
 taglib-property
 taglib-duration
 taglib-bitrate
 taglib-tags
 taglib-simple-tags
 )

;;;;
(require scheme/foreign)
(unsafe!)

(require srfi/2)
(require srfi/26)

(require (planet untyped/unlib/list))

;;; loading libraries
(define taglib 
  (if (equal? (system-type) 'windows)
      (ffi-lib "tag.dll")
      (ffi-lib "/usr/local/lib/libtag")))

(define taglib_c 
  (if (equal? (system-type) 'windows)
       (ffi-lib "tag_c.dll")
       (ffi-lib "/usr/local/lib/libtag_c")))

;;;
(define-syntax define-taglib
  (syntax-rules ()
    ((_ fn out-type in-types ...)
     (define fn 
       (get-ffi-obj (quote fn) taglib_c
                    (_fun in-types ... -> out-type))))))

(define-syntax define-pointer
  (syntax-rules ()
    ((_ ptr-type)
     (define ptr-type 
       (_cpointer/null (quote ptr-type))))))

;;;
(define-pointer _TagLib_File)
(define-pointer _TagLib_Tag)
(define-pointer _TagLib_AudioProperties)

;;;;;;;;;;;;;;;;;;
(define-taglib taglib_file_new             _TagLib_File            _bytes)
(define-taglib taglib_file_is_valid        _bool                   _TagLib_File)
(define-taglib taglib_file_tag             _TagLib_Tag             _TagLib_File)
(define-taglib taglib_file_audioproperties _TagLib_AudioProperties _TagLib_File)
(define-taglib taglib_file_free            _void                   _TagLib_File)

;;;
(define-taglib taglib_tag_title   _string/utf-8 _TagLib_Tag)
(define-taglib taglib_tag_artist  _string/utf-8 _TagLib_Tag)
(define-taglib taglib_tag_album   _string/utf-8 _TagLib_Tag)
(define-taglib taglib_tag_comment _string/utf-8 _TagLib_Tag)
(define-taglib taglib_tag_genre   _string/utf-8 _TagLib_Tag)
(define-taglib taglib_tag_year    _int32        _TagLib_Tag)
(define-taglib taglib_tag_track   _int32        _TagLib_Tag)

;;;
(define-taglib taglib_audioproperties_length     _int32 _TagLib_AudioProperties)
(define-taglib taglib_audioproperties_bitrate    _int32 _TagLib_AudioProperties)
(define-taglib taglib_audioproperties_samplerate _int32 _TagLib_AudioProperties)
(define-taglib taglib_audioproperties_channels   _int32 _TagLib_AudioProperties)

;;;
(define-taglib taglib_tag_free_strings _void)

(define (tag-number-or-false tag-function tags)
  (let ((n (tag-function tags)))
    (if (> n 0) n #f)))

;;;;;;;;;;;;;
(define tag->function-alist
  (list
   (cons 'artist      taglib_tag_artist)
   (cons 'album       taglib_tag_album)
   (cons 'title       taglib_tag_title)
   (cons 'date        (cut tag-number-or-false taglib_tag_year <>))
   (cons 'tracknumber (cut tag-number-or-false taglib_tag_track <>))))

(define (tag->function key)
  (or 
   (assoc-value/default key tag->function-alist #f)
   (raise (format "Unknown meta data field: ~A~%" key))))

(define property->function-alist
  (list
   (cons 'duration   taglib_audioproperties_length)
   (cons 'bitrate    taglib_audioproperties_bitrate)
   (cons 'samplerate taglib_audioproperties_samplerate)
   (cons 'channels   taglib_audioproperties_channels)))

(define (property->function property-tag)
  (or 
   (assoc-value/default property-tag property->function-alist #f)
   (raise (format "Unknown Property: ~A~%" property-tag))))

;;;;
(define (safe-taglib-file-new path)
  (with-handlers ((exn:fail:contract?
                   (lambda (e)
                     (printf "invalid latin-1 path: ~A~%" path)
                     #f)))
    (let ([b (string->bytes/latin-1 path)])
      (taglib_file_new b))))

(define (taglib-property path property-key)
  (and-let* ((f (safe-taglib-file-new path))
             ((taglib_file_is_valid f))
             (properties (taglib_file_audioproperties f)))    
    (begin0
      ((property->function property-key) properties)
      (taglib_file_free f))))

(define taglib-duration (cut taglib-property <> 'duration))
(define taglib-bitrate  (cut taglib-property <> 'bitrate))

(define (upcase-and-call key function data)
  (cons (string-upcase (symbol->string key)) 
        (function data)))

(define (taglib-properties path)
  (and-let* ((f (safe-taglib-file-new path))
             ((taglib_file_is_valid f))
             (properties (taglib_file_audioproperties f)))
    (begin0
      (dict-map property->function-alist (cut upcase-and-call <> <> properties))
      (taglib_file_free f))))

(define (taglib-tags path)
  (and-let* ((f (safe-taglib-file-new path))
             ((taglib_file_is_valid f))
             (tag (taglib_file_tag f)))
    (begin0
      (dict-map tag->function-alist (cut upcase-and-call <> <> tag))
      (taglib_tag_free_strings)
      (taglib_file_free f))))

(define (taglib-simple-tags path . tag-list)
  (and-let* ((f (safe-taglib-file-new path))
             ((taglib_file_is_valid f))
             (tag (taglib_file_tag f)))
    (begin0
      (for/list ((tag-key tag-list))
        ((tag->function tag-key) tag))
      (taglib_tag_free_strings)
      (taglib_file_free f))))

;;;(taglib-tags "etc/test2.mp3")
