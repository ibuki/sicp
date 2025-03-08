#lang racket
(define (square x)
  (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part) x]
      [(eq? op 'imag-part) y]
      [(eq? op 'magnitude) (sqrt (+ (square x) (square y)))]
      [(eq? op 'angle) (atan y x)]
      [else (error " Unknown op: MAKE-FROM-REAL-IMAG " op)]))
  dispatch)

(define (apply-generic op arg)
  (arg op))

(apply-generic 'real-part (make-from-real-imag 1 1))
(apply-generic 'magnitude (make-from-real-imag 1 1))

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part) (* x (cos y))]
      [(eq? op 'imag-part) (* x (sin y))]
      [(eq? op 'magnitude) x]
      [(eq? op 'angle) y]
      [else (error " Unknown op: MAKE-FROM-REAL-IMAG " op)]))
  dispatch)

(apply-generic 'real-part (make-from-mag-ang 1 (atan -1)))
(apply-generic 'magnitude (make-from-mag-ang 1 (atan -1)))
