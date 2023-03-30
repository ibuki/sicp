#lang racket

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((x (expmod base (/ exp 2) m)))
          (if (non-trivial-sqrt? x m) 0 (remainder (square x) m))))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

(define (non-trivial-sqrt? n m)
  (cond ((= n 1) false)
        ((= n (- m 1)) false)
        (else (= 1 (remainder (square n) m)))))
;;; (expmod 1 1 7)
;;; (expmod 2 1 7)
;;; (expmod 2 1 2)
;;; (expmod 2 1 2)

;;; (define (loop-itr n i process)
;;;   (process)
;;;   (cond (< i n) (loop-itr n (+ i 1) process))
;;; )
;;; (define (loop n process)
;;;   (loop-itr n 0 process))

;;; (expmod 1 2 3)
;;; (expmod 2 2 3)
;;; (expmod 3 2 3)
;;; (expmod 4 2 3)
;;; (expmod 5 2 3)
;;; (expmod 6 2 3)
;;; (expmod 7 2 3)

(define (miller-rabin n)
  (miller-rabin-test (- n 1) n))

(define (miller-rabin-test a n)
  (cond ((= a 0) true)
        ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n))
        (else false)))

(miller-rabin 561)
(miller-rabin 1008)
(miller-rabin 1009)

(miller-rabin 1000037)
