#lang racket

(define (fib2 n)
  (fib-iter2 1 0 0 1 n))
(define (fib-iter2 a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter2 a
                    b
                    (+ (square p) (square q))
                    (+ (* 2 p q) (square q))
                    (/ count 2)))
        (else (fib-iter2 (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

(define (square x) (* x x))

;;; (define (fib n) (fib-iter 1 0 n))
;;; (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
;;; (fib2 500000)
;;; (fib 500000)
