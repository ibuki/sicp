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

(define (fast-prime? n times)
  (cond ((= times 0) true)
      ((fermat-test n) (fast-prime? n (- times 1))) (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

;;; (search-for-nth-prime 1001 3)
;;; (search-for-nth-prime 10001 3)
;;; (search-for-nth-prime 100001 3)
;;; (search-for-nth-prime 1000001 3)

(fast-prime? 1009 1000)
(fast-prime? 1013 1000)
(fast-prime? 1019 1000)
(fast-prime? 10007 1000)
(fast-prime? 10009 1000)
(fast-prime? 10037 1000)
(fast-prime? 100003 1000)
(fast-prime? 100019 1000)
(fast-prime? 100043 1000)
(fast-prime? 1000003 1000)
(fast-prime? 1000033 1000)
(fast-prime? 1000037 1000)

(fast-prime? 561 1000)
(fast-prime? 1105 1000)

;;; (fast-prime? 10000000000037 1000)
;;; (fast-prime? 10000000000051 1000)
;;; (fast-prime? 10000000000099 1000)
;;; (fast-prime? 100000000000031 1000)
;;; (fast-prime? 100000000000067 1000)
;;; (fast-prime? 100000000000097 1000)
;;; (fast-prime? 1000000000000037 1000)
;;; (fast-prime? 1000000000000091 1000)
;;; (fast-prime? 1000000000000159 1000)
;;; (fast-prime? 10000000000000061 1000)
;;; (fast-prime? 10000000000000069 1000)
;;; (fast-prime? 10000000000000079 1000)
