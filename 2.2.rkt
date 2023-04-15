#lang racket

(define (average x1 x2) (/ (+ x1 x2) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; (define (print-segment segment)
;;;   (newline)
;;;   (display "start:")
;;;   (print-point (start-segment segment))
;;;   (display "end:")
;;;   (print-point (end-segment segment)))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment s)
  (let ((start (start-segment s)) (end (end-segment s)))
    (make-point (average (x-point start) (x-point end)) (average (y-point start) (y-point end)))))

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define target-segment (make-segment (make-point -3 0) (make-point 2 3)))
(print-point (midpoint-segment target-segment))
