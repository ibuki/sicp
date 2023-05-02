#lang racket

(define (square x)
  (* x x))

;;; (define (square-list1 items)
;;;   (if (null? items) '() (cons (square (car items)) (square-list1 (cdr items)))))

;;; (define (square-list2 items)
;;;   (map square items))

;;; (square-list1 (list 1 2 3 4 5))
;;; (square-list2 (list 1 2 3 4 5))

(define (square-tree tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (square tree)]
    [else (cons (square-tree (car tree)) (square-tree (cdr tree)))]))

(define (map-tree f tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (f tree)]
    [else (cons (map-tree f (car tree)) (map-tree f (cdr tree)))]))

(define (square-tree2 tree)
  (map-tree square tree))


(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
