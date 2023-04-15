#lang racket
(define (power base n) (if (= n 0) 1 (* base (power base (- n 1)))))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car value)
  (dividable-count 2 value))
(define (cdr value)
  (dividable-count 3 value))

(define (dividable-count a value)
  (define (itr value divided-count)
    (if (= 0 (remainder value a)) (itr (/ value a) (+ divided-count 1)) divided-count))
  (itr value 0))

(car (cons 123 124))
(cdr (cons 123 124))
