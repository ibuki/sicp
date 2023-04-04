#lang racket

(define (inc x) (+ x 1))
(define (d f)
  (lambda (x) (f (f x))))
(((d (d d)) inc) 5)

(define dd (d d))

(((d (d d)) inc) 5)
(((d dd) inc) 5)
((dd (dd inc)) 5)
(((d d) ((d d) inc)) 5)
(((d d) (d (d inc))) 5)
((d (d (d (d inc)))) 5)
