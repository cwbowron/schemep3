#lang scheme/base

(provide with-finalized-binding)

(require srfi/2)

(define-syntax with-finalized-binding 
  (syntax-rules ()
    ((_ initial-value-thunk value-thunk finalize-thunk)
     (and-let* ((v (initial-value-thunk)))
       (dynamic-wind
        void
        (lambda () (value-thunk v))
        (lambda () (finalize-thunk v)))))))
