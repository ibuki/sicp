#lang racket

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(= x (entry set)) true]
    [(< x (entry set)) (element-of-set? x (left-branch set))]
    [(> x (entry set)) (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond
    [(null? set) (make-tree x '() '())]
    [(= x (entry set)) set]
    [(< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set))]
    [(> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set)))]))
(define (make-entry x)
  (make-tree x '() '()))

(adjoin-set 1 '())
(adjoin-set 2 (adjoin-set 1 '()))
(define (adjoin-set-multiple i n set)
  (if (> i n)
      set
      (adjoin-set-multiple (+ i 1) n (adjoin-set i set))))
(define (adjoin-set-multiple7)
  (adjoin-set-multiple 1 7 '()))
(adjoin-set-multiple7)
(define (tree->2.16.1)
  (make-tree 7
             (make-tree 3 (make-entry 1) (make-entry 5))
             (make-tree 9 '() (make-entry 11))))
(define (tree->2.16.2)
  (make-tree 3
  (make-entry 1)
  (make-tree 7 (make-entry 5)
             (make-tree 9 '() (make-entry 11)))))
(define (tree->2.16.3)
  (make-tree 5
             (make-tree 3 (make-entry 1) '())
             (make-tree 9 (make-entry 7) (make-entry 11))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(tree->list-1 (adjoin-set-multiple7))
(tree->list-2 (adjoin-set-multiple7))

(tree->list-1 (tree->2.16.1))
(tree->list-1 (tree->2.16.2))
(tree->list-1 (tree->2.16.3))
