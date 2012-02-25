#lang scheme

(provide/contract
 [format-date            (-> date? string?)]
 [format-time            (-> integer? string?)]
 [ensure-string          (-> any/c string?)]
 [timestamp->seconds     (-> string? integer?)]
 [padded-string          (-> string? fixnum? string?)]
 [get-tags               (-> (or/c string? path?) any)]
 [list-remove-item       (-> list? exact-nonnegative-integer? list?)]
 [clip-string            (-> string? exact-nonnegative-integer? string?)]
 [boolify                (-> any/c boolean?)]
 [number->integer        (-> number? exact-integer?)]
 
 [exit:insert-on-callback-with-status (-> procedure? (or/c false/c string?) void)]
 [thread-send/ignore     (-> (or/c false/c thread?) any/c void)]
 
 [list-ref/default       (-> list? exact-nonnegative-integer? any/c any/c)]
 [car/default               (-> pair? any/c any/c)]

 [filename-extensions-regex (-> (listof string?) regexp?)]
 )

(provide 
 push!
 add-hook!
 define-hook
 define-unnamed-hook
 call-hooks
 thread-send-and-wait
 thread-set!
 define-lazy-singleton
 define-singleton
 send/ignore
 complete-or
 find-tag
 
 path-or-string->path
 path-or-string->string
 
 preferences-backed-variable
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;
(require scheme/date)
(require framework)
(require srfi/2)
(require srfi/54)

(require (planet untyped/unlib/list))

(require "taglib.ss")

(define (exit:insert-on-callback-with-status callback (message #f))
  (exit:insert-on-callback
   (lambda () 
     (when message
       (fprintf (current-error-port) "exiting: ~A~%" message))
     (callback))))

(define (list-ref/default the-list n default-value)
  (if (> (length the-list) n)
      (list-ref the-list n)
      default-value))

(define (car/default the-list default-value)
  (if (null? the-list) default-value (car the-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-tags file)
  (taglib-tags file))

(define (find-tag tag-list tag)
  (assoc-value/default tag tag-list #f)) 

(define-syntax send/ignore 
  (syntax-rules ()
    ((_ obj z ...)     
     (when obj (send obj z ...)))))

(define-syntax push!
  (syntax-rules () ((_ the-list atom) (set! the-list (cons atom the-list)))))

(define (list-remove-item the-list n)
  (let-values ([(head tail)
                (split-at the-list n)])
    (append head (cdr tail))))

(define-syntax add-hook!
  (syntax-rules ()
    ((_ name hook) (push! name hook))))

(define-syntax define-hook
  (syntax-rules ()
    ((_ hook-name)
     (define hook-name (list)))
    ((_ hook-name hook-add-fn)
     (begin
       (define-hook hook-name)
       (define (hook-add-fn hook)
         (add-hook! hook-name hook))))
    ((_ hook-name hook-add-fn hook-call-fn)
     (begin
       (define-hook hook-name hook-add-fn)
       (define (hook-call-fn . args)
         (apply call-hooks hook-name args))))))

(define-syntax define-unnamed-hook
  (syntax-rules ()
    ((_ add-fn call-fn)
     (define-values
       (add-fn call-fn)
       (let ([hooks (list)])
         (values
          (lambda (hook)
            (add-hook! hooks hook))
          (lambda p
            (apply call-hooks hooks p))))))))

(define (call-hooks hooks . args)
  (for ((hook hooks))
    (apply hook args)))

(define (clip-string string max-length)
  (if (< (string-length string) max-length)
      string
      (substring string 0 (- max-length 1))))

(define (padded-string s length)
  (let ((abs-length (abs length))
        (len (string-length s)))
    (cond     
      ((= len abs-length) s)
      ((< len abs-length) 
       (cat s length))
      (else (substring s 0 abs-length)))))

(define (format-date date)
  (if (< (date-year date) 1970)
      "----------"
      (parameterize ([date-display-format 'iso-8601])
        (date->string date))))

(define (format-date-seconds seconds)
  (format-date (seconds->date seconds)))

(define (ensure-number n)
  (if (string? n) 
      (string->number n)
      n))

(define (ensure-string var)
  (format "~A" var))

(define (format-time seconds)
  (let ((sec (number->integer (ensure-number seconds))))
    (let ((m (floor (/ sec 60)))
          (s (modulo sec 60)))
      (format "~A:~A" m (cat s 2 #\0)))))

(define (timestamp->seconds timestamp-string)
  (and-let* ((m (regexp-match 
                 (pregexp "(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)")
                 timestamp-string))
             (year (string->number (second m)))
             (month (string->number (third m)))
             (day (string->number (fourth m)))
             (date-seconds (find-seconds 0 0 0 day month year)))
    date-seconds))

(define (boolify x)
  (if x #t #f))

(define (number->integer n)
  (inexact->exact (round n)))

(define-syntax thread-set!
  (syntax-rules ()
    ((_ th fn)
     (set! th (thread (lambda () (fn) (set! th #f)))))))
     
(define (thread-send/ignore th msg)
  (when (and th (thread-running? th))
    (thread-send th msg)))

(define (thread-send-and-wait th . msgs)
  (when (and th (thread-running? th))
    (for ((msg msgs))
      (thread-send th msg))
    (thread-wait th)))

(define-syntax define-lazy-singleton
  (syntax-rules ()
    ((_ accessor class)
     (begin
       (define accessor
         (let ((the-instance #f))
           (lambda ((force-create #f))
             (when (and force-create (not the-instance))
               (set! the-instance (new class)))
             the-instance)))))))

(define-syntax define-singleton
  (syntax-rules ()
    ((_ accessor class)
     (begin       
       (define accessor
         (let ((the-instance (new class)))
           (lambda ()
             the-instance)))))))

;;;;
(define (filename-extension-regex ext)
  (string-append
   (regexp-quote ext #f)
   "$"))

(define (filename-extensions-regex ext-list)
  (regexp
   (string-join 
    (map filename-extension-regex ext-list)
    "|")))

(define-syntax complete-or 
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 e3 ...)
     (let ((t e1))
       (if t 
	   (begin0 t e2 e3 ...)
	   (complete-or e2 e3 ...))))))

(define (path-or-string->string path-or-string)
  (if (path? path-or-string)
      (path->string path-or-string)
      path-or-string))

(define (path-or-string->path path-or-string)
  (if (string? path-or-string)
      (string->path path-or-string)
      path-or-string))

(define (preferences-backed-variable key default-value)
  (preferences:set-default key default-value (lambda (x) #t))
  (lambda ((new-value #f))
    (when new-value
      (preferences:set key new-value))
    (preferences:get key)))
