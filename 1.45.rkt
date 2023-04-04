#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
      tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average v1 v2) (/ (+ v1 v2) 2))
(define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; (define (sqrt x)
;;;   (fixed-point (average-damp (lambda (y) (/ x y)))
;;;     1.0))

;;; (define (cube-root x)
;;;   (fixed-point (average-damp (lambda (y) (/ x (square y))))
;;;     1.0))

;;; (cube-root 27)

(define (cube x) (* x x x))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (repeated-average-damp n)
  (repeated average-damp n))

(define (power x n)
  (if (= n 1) x (* x (power x (- n 1)))))

(define (nth-root n x damp)
  (fixed-point (damp (lambda (y) (/ x (power y (- n 1)))))
    1.0))

(nth-root 3 27 average-damp)
(nth-root 4 (power 3 4) (repeated-average-damp 2))
(nth-root 5 (power 3 5) (repeated-average-damp 2))
(nth-root 6 (power 3 6) (repeated-average-damp 2))
(nth-root 7 (power 3 7) (repeated-average-damp 2))
(nth-root 8 (power 3 8) (repeated-average-damp 3))
(nth-root 9 (power 3 9) (repeated-average-damp 3))
(nth-root 100 (power 3 100) (repeated-average-damp 4))
(nth-root 101 (power 3 101) (repeated-average-damp 4))
