#lang racket

;;; (define (tan-cf x k)
;;;   (let ((xx (* x x)))
;;;     (define (calc i)
;;;       (cond ((> i k) 0)
;;;         (else (/ xx (- (- (* 2.0 i) 1) (calc (+ i 1)))))))
;;;     (/ x (calc 0))))


(define (cont-frac-fast n d k)
  (define (itr i a)
    (cond ((= i 0) a)
      (else (itr (- i 1) (/ (n i) (+ (d i) a))))))
  (itr k 0))

(define (tan-cf-fast x k)
  (cont-frac-fast
    (lambda (i) (if (= i 1) x (* x x -1)))
    (lambda (i) (- (* 2.0 i) 1))
    k))


(define x 1)
(display (tan-cf-fast x 8)) (newline)
(display (tan x)) (newline)
