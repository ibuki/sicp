#lang racket

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
      ((filter a) (iter (next a) result))
      (else (iter (next a) (combiner result (term a))))))
  (iter a null-value))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (product n)
  (filtered-accumulate odd? * 1 identity 1 inc n))

(define (sum n)
  (filtered-accumulate odd? + 0 identity 1 inc n))

(product 6)
(sum 11)
