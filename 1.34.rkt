#lang racket
;;; (define (plus4 x) (+ x 4))
;;; (define plus4-2 (lambda (x) (+ x 4)))
;;; (plus4-2 1)

(define (square x) (* x x))
(define (f g) (g 2))


(f (f 2))
