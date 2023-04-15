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

;;; (define target-segment (make-segment (make-point -3 0) (make-point 2 3)))
;;; (print-point (midpoint-segment target-segment))

;;; (define (make-rect p1 p2 p3 p4)
;;;   (make-segment (make-point p1 p3) (make-point p2 p4)))
;;; (define (p1-rect rect) (cdr (cdr rect)))
;;; (define (p2-rect rect) (cdr (car rect)))
;;; (define (p3-rect rect) (car (cdr rect)))
;;; (define (p4-rect rect) (car (car rect)))

(define (width-rect rect)
  (abs (- (x-point (p2-rect rect)) (x-point (p1-rect rect)))))
(define (height-rect rect)
  (abs (- (y-point (p1-rect rect)) (y-point (p4-rect rect)))))
(define (circumference-rect rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))
(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

(define (make-rect p1 p2 p3 p4)
  (cons p1 (cons p2 (cons p3 p4))))
(define (p1-rect rect) (car rect))
(define (p2-rect rect) (car (cdr rect)))
(define (p3-rect rect) (car (cdr (cdr rect))))
(define (p4-rect rect) (cdr (cdr (cdr rect))))


(define rect1
  (make-rect (make-point 1 3)
             (make-point 4 3)
             (make-point 4 1)
             (make-point 1 1)))

(circumference-rect rect1)
(area-rect rect1)
