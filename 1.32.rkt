#lang racket

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (product n)
  (accumulate * 1 identity 1 inc n))

(define (sum n)
  (accumulate + 0 identity 1 inc n))

(product 6)
(sum 11)
