#lang scheme/gui

(provide/contract
 [get-medium-mono-font (-> (is-a?/c font%))]
 (get-large-mono-font  (-> (is-a?/c font%)))
 (ignoring-inputs      (-> procedure? procedure?))
 )

(define (get-medium-mono-font)
  (make-object font% 
    (if (equal? (system-type) 'macosx) 12 9)
    'modern))

(define (get-large-mono-font)
  (make-object font% 
    (if (equal? (system-type) 'windows)
        12
        16)
    'modern))

(define (ignoring-inputs fn)
  (lambda x
    (fn)))
