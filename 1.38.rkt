#lang racket

(define (cont-frac-fast n d k)
  (define (itr i a)
    (cond ((= i 0) a)
      (else (itr (- i 1) (/ (n i) (+ (d i) a))))))
  (itr k 0))

(define (divides? a b)
  (= (remainder b a) 0))

(define (d1 i)
  (if (divides? 3 (+ i 1)) (* (/ (+ i 1) 3) 2) 1))

(+ 2
(cont-frac-fast (lambda (i) 1.0)
    d1
    20000000)
)
