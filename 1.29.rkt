#lang racket

(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))

(define (inc2 i) (+ i 2))

(define (symp f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (* (/ h 3) (+ (y 0) (y n) (* 4 (sum y 1 inc2 n)) (* 2 (sum y 2 inc2 (- n 3))))))

(define (cube x) (* x x x))
(symp cube 0 1 10000)
