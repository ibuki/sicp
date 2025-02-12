#lang racket

(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(= x (car set)) true]
    [(< x (car set)) false]
    [else (element-of-set? x (cdr set))]))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond
          [(= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2)))]
          [(< x1 x2) (intersection-set (cdr set1) set2)]
          [(< x2 x1) (intersection-set set1 (cdr set2))]))))

(intersection-set '(3 2 1) '(1 2 4))

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(element-of-set? x set) set]
    [(< x (car set)) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

(adjoin-set 3 '())
(adjoin-set 0 '(1 2 4))
(adjoin-set 2 '(1 2 4))
(adjoin-set 3 '(1 2 4))
(adjoin-set 5 '(1 2 4))
