#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; (adjoin-set 3 '(1 2 4))
; (adjoin-set 3 '(1 2 4))
(intersection-set '(3 2 1) '(1 2 4))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '() '(2 3))
(union-set '(1 2) '())
(union-set '(1 2) '(2 3))
(union-set '(1 2) '(1 3))
