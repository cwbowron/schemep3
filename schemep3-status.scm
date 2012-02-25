#lang scheme

(provide/contract 
 [status:add-hook (-> procedure? void)]
 [status:update   (->* (string?) () #:rest any/c void)])

(provide
 progress-bar:add-hook
 progress-bar:show
 progress-bar:hide
 progress-bar:set
 progress-bar:set-status
 for/progress
 )

(require "schemep3-helpers.scm")

;;; status
(define-unnamed-hook status:add-hook status:call-hooks)

(define (status:update . format-params)
  (status:call-hooks (apply format format-params)))

;;;  progress bar status
(define-unnamed-hook progress-bar:add-hook progress-bar:call-hooks)

(define (progress-bar:show)
  (progress-bar:call-hooks 'show))

(define (progress-bar:hide)
  (progress-bar:call-hooks 'hide))

(define (progress-bar:set n)
  (progress-bar:call-hooks 'set n))

(define (progress-bar:set-status status)
  (progress-bar:call-hooks 'set-status status))

(define-syntax for/progress
  (syntax-rules ()
    ((_ ((N M) bindings ...) body ...)
     (let ([MAX M])
       (progress-bar:show)
       (progress-bar:set 0)
       (for (bindings ...
             (N (in-naturals 1)))
         (begin body ...
                (progress-bar:set (number->integer (* 100 (/ N MAX))))))
       (progress-bar:hide)))))
