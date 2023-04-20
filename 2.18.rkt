#lang racket

(define (reverse l)
  (define (reverse-itr l1 l2)
    (if (null? l1) l2 (reverse-itr (cdr l1) (cons (car l1) l2))))
  (reverse-itr l (list)))

(reverse (list 23 72 149 34))
