#lang scheme

(provide/contract
 [convert-audio-file (-> path-string? path-string? (one-of/c 'mp3) boolean?)]
 )

(require scheme/file)
(require scheme/system)
(require srfi/2)

(require (planet untyped/unlib/list))
(require "schemep3-helpers.scm")

;;; MPlayer [generic decode]
(define mplayer:executable
  (if (equal? (system-type) 'windows)
      (path->string 
       (build-path
        (current-directory)
        "bin"
        "mplayer.exe"))
      "/opt/local/bin/mplayer"))

(define (mplayer:decode source-file output-directory)
  (unless (directory-exists? output-directory)
    (printf "Creating Directory: ~A~%" output-directory)
    (make-directory* output-directory))
  (let* ((tmp-file (make-temporary-file "schemep3-~A.pcm" #f output-directory))
         (tmp-file-name (file-name-from-path tmp-file))
         (command-line
          (format "~A -really-quiet -vc null -vo null -ao pcm:file=\"~A\" \"~A\"" 
                  mplayer:executable tmp-file-name source-file)))
    ;;;(printf "~A~%" command-line)
    (printf "Decoding to ~A~%" tmp-file)    
    (parameterize ([current-directory output-directory])
      (system command-line))
    tmp-file))

;;; Lame [mp3 encoder]
(define lame:executable
  (if (equal? (system-type) 'windows)
      "bin\\lame.exe"
      "/opt/local/bin/lame"))

(define lame:tag-map
  `(("TITLE"       . "tt")
    ("ARTIST"      . "ta")
    ("ALBUM"       . "tl")
    ("DATE"        . "ty")
    ("TRACKNUMBER" . "tn")))

(define (lame:lookup-tag tag-pair)
  (and-let* ((lame-flag (assoc-value/default (car tag-pair) lame:tag-map #f)))
    (format "--~A \"~A\"" lame-flag (cdr tag-pair))))

(define (lame:build-options tags)
  (string-join (filter-map lame:lookup-tag tags) " "))

(define (lame:mp3-encode pcm-file mp3-file bitrate tags)
  (let ((command-line
         (format "~A -S -b ~A ~A \"~A\" \"~A\"" 
                 lame:executable
                 bitrate
                 (lame:build-options tags)
                 pcm-file 
                 mp3-file)))
    ;;;(printf "~A~%" command-line)
    (printf "Encoding to ~A~%" mp3-file)
    (system command-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (convert-audio-file source-file target-file target-type)
  (printf "~A -> ~A~%" source-file target-file)
  ;;; decode to temporary wave
  (let ((pcm-file (mplayer:decode source-file (path-only target-file)))
        (tags (get-tags source-file)))
    ;;; compress to target...
    (case target-type
      ((mp3)
       (lame:mp3-encode pcm-file target-file 320 tags)))
    ;;; delete temporary file
    (unless (equal? source-file pcm-file)
      (delete-file pcm-file))
    #t))

;(require "in-alist.ss")
;
;(for ((file (in-files "E:\\Music\\Library\\L\\Laibach\\1987 - Opus Dei [wavpack]"))
;      #:when (equal? #"wv" (filename-extension file)))
;  (let ([o (build-path "E:\\Music\\Library\\L\\Laibach\\1987 - Opus Dei [wavpack]"
;                       (regexp-replace ".wv" (path->string (file-name-from-path file)) ".mp3"))])
;    ;;;(printf "~A => ~A~%" file o)
;    (convert-audio-file file o 'mp3)))

;;(convert-audio-file "C:/tmp/test.flac"
;;                    "c:/tmp/test.mp3" 'mp3)
;;
;;(let ((s (build-lame-options '(("ARTIST" . "A") ("TITLE" . "T") ("ALBUM" . "ALB") ("DATE" . "2000")))))
;;  (printf "build-lames-options returned: ~S~%" s)
;;  (equal? s "--tt \"T\" --ta \"A\" --tl \"ALB\" --ty \"2000\""))