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

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
    1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
    1.0))

;;; (cube-root 27)

(define (cube x) (* x x x))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

;;; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))
(sqrt 10000)
(sqrt2 10000)
