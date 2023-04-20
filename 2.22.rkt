#lang racket
;;; (define (for-each proc lst)
;;;   (proc (car lst))
;;;   (if (null? (cdr lst)) #t (for-each proc (cdr lst))))
(define (for-each proc lst)
  (if (null? lst) #t ((lambda () (proc (car lst)) (for-each proc (cdr lst))))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
