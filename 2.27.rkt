#lang racket

(define (reverse l)
  (define (reverse-itr l1 l2)
    (if (null? l1) l2 (reverse-itr (cdr l1) (cons (car l1) l2))))
  (reverse-itr l (list)))

(reverse (list 23 72 149 34))

(define x (list (list 1 2) (list 3 4)))
(reverse x)

(define (deep-reverse l)
  (define (deep-reverse-itr l1 l2)
    (if (null? l1) l2 (deep-reverse-itr (cdr l1) (cons (deep-reverse (car l1)) l2))))
  (if (pair? l) (deep-reverse-itr l (list)) l))

(deep-reverse x)
