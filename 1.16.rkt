#lang racket
(define (square n) (* n n))

(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-itr a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-itr a (square b) (/ n 2)))
        (else (fast-expt-itr (* a b) b (- n 1)))))

(define (fast-expt2 b n)
  (fast-expt-itr 1 b n))


(fast-expt 3 1000)
(fast-expt2 3 1000)
