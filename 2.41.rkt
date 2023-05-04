#lang racket

(define nil null)
(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high) null (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (flat proc seq)
  (accumulate append nil seq))

(define (sum? sum trio)
  (= sum (accumulate + 0 trio)))

(define (sum-trios n sum)
  (filter (lambda (trio) (sum? sum trio)) (unique-trios n)))

(define (unique-trios n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1)))) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-trios 6)
(sum-trios 6 10)
