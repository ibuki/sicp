#lang racket

(define (iterative-improve enough? improve)
  (define (iter guess)
    (if (enough? guess) guess (iter (improve guess))))
  (lambda (guess) (iter guess)))

  ;;; (lambda (x)
  ;;;   (let ((guess (improve x)))
  ;;;     (if (enough? guess) guess (iterative-improve enough? improve))
  ;;;   )
  ;;; (lambda (x) (if (enough? x) x (improve x))))

;;;sqrt basic
(define (sqrt-iter guess x)
 (if (good-enough? guess x)
    guess
    (sqrt-iter (improve1 guess x) x)))
(define (improve1 guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 5)

;;; sqrt iterative-imporove

(define (good-enough?2 x)
  (lambda (guess)
    (< (abs (- (square guess) x)) 0.00001)))
(define (improve2 x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (sqrt2 x)
  ((iterative-improve (good-enough?2 x) (improve2 x)) 1.0))
(sqrt2 5.0)

;;; fixed-point basic

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

;;; (define (average v1 v2) (/ (+ v1 v2) 2))
;;; (define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt3 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
    1.0))

(sqrt3 5)

;;; fixed-point iterative-imporove

(define (sqrt4 x)
  (fixed-point2 (average-damp (lambda (y) (/ x y)))
    1.0))

(define (fixed-point2 f first-guess)
  (define (enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve enough? f) first-guess))

(sqrt4 5)
