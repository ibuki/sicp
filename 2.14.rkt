#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (make-center-width c (* c p 2)))
(define (percent i)
  (/ (/ (width i) 2) (center i)))

(define (print-interval x)
  (newline)
  (display (lower-bound x))
  (display "...")
  (display (upper-bound x))
  (display " or ")
  (display (center x))
  (display "±")
  (display (/ (width x) 2))
  )

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
       (div-interval
        one (add-interval (div-interval one r1)
                          (div-interval one r2)))))

;;; (define interval1 (make-interval 99.998 100))
;;; (define interval2 (make-interval 99.997 99.999))
(define interval1 (make-interval 1 1))
(define interval2 (make-interval 1 2))

;;; (print-interval (add-interval interval1 interval2))
;;; (print-interval (sub-interval interval1 interval2))
;;; (print-interval (mul-interval interval1 interval2))
;;; (print-interval (div-interval interval1 interval2))

;;; (print-interval (div-interval interval1 interval1))
;;; (print-interval (div-interval interval1 interval2))
;;; (print-interval (par1 interval1 interval2))
;;; (print-interval (par2 interval1 interval2))

(print-interval (div-interval interval1 interval2))
(print-interval (div-interval interval1 (div-interval interval1 interval2)))
