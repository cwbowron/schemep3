#lang scheme/base

(require scheme/contract)
(require srfi/26)

(provide/contract
 [in-alist (-> (listof pair?) sequence?)]
 [in-files (-> path-string? sequence?)]
 [in-path  (-> path-string? sequence?)]
 )

(define (in-alist alist)
  (make-do-sequence
   (lambda ()
     (values 
      (lambda (pos)
        (let ((pair (car pos)))
          (values (car pair) (cdr pair))))
      cdr
      alist
      (lambda (pos) (not (null? pos)))
      (lambda x #t)
      (lambda x #t)))))

;;;(define a '((a . 1) (b . 2) (c . (1 2 3 4 5))))
;;;(for (((key value) (in-alist a)))
;;;  (printf "key = ~A, value = ~A~%" key value))

(define (directory-or-file-list path)
  (if (directory-exists? path)
      (map (cut build-path path <>) (directory-list path))
      (list path)))

(define (in-path path)
  (make-do-sequence
   (lambda ()
     (values 
      (lambda (p)
        (let ([full (car p)])
          (cond
            [(directory-exists? full)
             (values full 'dir)]
            [(link-exists? full)
             (values full 'link)]
            [(file-exists? full)
             (values full 'file)]
            [else 
             (raise "error in in-files")])))
      cdr
      (directory-or-file-list path)
      (lambda (p) (not (null? p)))
      (lambda x #t)
      (lambda x #t)))))

(define (in-files path)
  (letrec ([next-file 
            (lambda (p)
              (cond [(null? p) p]
                    [(file-exists? (car p)) p]
                    [(directory-exists? (car p))
                     (let ([more (map (cut build-path (car p) <>) (directory-list (car p)))])
                       (next-file (append (cdr p) more)))]
                    [else
                     (next-file (cdr p))]))])     
    (make-do-sequence
     (lambda ()
       (values 
        (lambda (p)
          (unless (file-exists? (car p))
            (raise "in-files:Poop"))
          (car p))
        (lambda (p)
          (next-file (cdr p)))
        (next-file (directory-or-file-list path))
        (lambda (p) (not (null? p)))
        (lambda x #t)
        (lambda x #t))))))
  
;;;(for ((f (in-files "c:\\tmp\\")))
;;;  (printf "f = ~A~%" f))
