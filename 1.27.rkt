#lang racket

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

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


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (check a n)
  (display (expmod a n n))
  (newline))

(define (check-itr n i)
  (check i 1009)
  (cond ((< i n) (check-itr n (+ i 1)))))

(check-itr 1008 1)
