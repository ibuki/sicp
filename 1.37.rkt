#lang racket
;;; (/ n1 (+ d1 (/ n2 (+ d2 (/ n3 (+ d3 0))))))

;;; (define (cont-frac n d k)
;;;   (define (itr n d k i)
;;;     (/ (n i) (+ (d i) ))

;;;   ))

(define (cont-frac n d k)
  (define (calc i)
    (cond ((> i k) 0)
      (else (/ (n i) (+ (d i) (calc (+ i 1)))))))
  (calc 0))

(define (cont-frac-fast n d k)
  (define (itr i a)
    (cond ((= i 0) a)
      (else (itr (- i 1) (/ (n i) (+ (d i) a))))))
  (itr k 0))

(cont-frac-fast (lambda (i) 1.0)
    (lambda (i) 1.0)
    20000000)

(cont-frac (lambda (i) 1.0)
    (lambda (i) 1.0)
    20000000)

(cont-frac-fast (lambda (i) 1.0)
    (lambda (i) 1.0)
    20000000)
