#lang racket

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (= (remainder x 2) 1))

(define (filter lst method)
  (cond
    [(null? lst) '()]
    [(method (car lst)) (cons (car lst) (filter (cdr lst) method))]
    [(filter (cdr lst) method)]))

(define (same-parity . lst)
  (if (odd? (car lst)) (filter lst odd?) (filter lst even?)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
