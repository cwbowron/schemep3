#lang scheme

(provide twitter)

(require net/base64)
(require net/url)

(define (strip-crlf b64-bytes)
  (subbytes b64-bytes 0 (- (bytes-length b64-bytes) 2)))

;;; note that this uses the very insecure basic authorization method...
(define (twitter username password status)
  (let* ([basic-auth (strip-crlf (base64-encode (string->bytes/utf-8 (string-append username ":" password))))])
    (let ([url 
           (make-url
            "http"
            #f
            "twitter.com"
            80
            #t
            (list 
             (make-path/param "statuses" (list))
             (make-path/param "update.xml" (list)))
            (list
             (cons 'status status))
            #f)])
      ;;;(printf "Twittering: ~A~%" status)
      (let ([port 
             (post-impure-port
              url
              #""
              (list
               (string-append "Authorization: Basic " (bytes->string/utf-8 basic-auth))))])
        ;;;(for ((line (in-lines port)))
        ;;;  (printf "<-- ~A~%" line))
        (close-input-port port)))))
