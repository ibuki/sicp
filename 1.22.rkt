#lang racket

(define (smallest-divisor n)
  (find-divisor n 2)
)

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define runtime current-milliseconds)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  n)

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes target)
  (if (prime? target) (timed-prime-test target) (search-for-primes (+ target 2))))

(define (search-for-nth-prime target n)
  (if (= n 1) (search-for-primes target) (search-for-nth-prime (+ 2 (search-for-primes target)) (- n 1)))
  (newline)
  )



(search-for-nth-prime 10000000000001 3)
(search-for-nth-prime 100000000000001 3)
(search-for-nth-prime 1000000000000001 3)
(search-for-nth-prime 10000000000000001 3)
