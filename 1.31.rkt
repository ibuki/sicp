#lang racket

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product2 term a next b)
  (if (> a b) 1
    (* (term a) (product2 term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (factorial n) (product identity 1 inc n))
(factorial 7)

(define (square x) (* x x))
(define (pi n)
  (define (term x) (/ (* (* x 2) (* (+ x 1) 2)) (square (+ (* x 2) 1))))
  (* 4 (product2 term 1 inc n)))
(pi 10)
(pi 100)

(exact->inexact (pi 100))
(exact->inexact (pi 1000))
(exact->inexact (pi 10000))
