#lang racket

(define (key x) x)

(define (lookup given-key set-of-records)
  (cond
    [(null? set-of-records) false]
    [(equal? given-key (key (car set-of-records))) (car set-of-records)]
    [else (lookup given-key (cdr set-of-records))]))

(lookup 3 '(1 3 5 7))

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-entry x)
  (make-tree x '() '()))
(define (tree->2.16.1)
  (make-tree 7 (make-tree 3 (make-entry 1) (make-entry 5)) (make-tree 9 '() (make-entry 11))))


(define (lookup-tree given-key tree-of-records)
  (cond
    [(null? tree-of-records) false]
    [(equal? given-key (key (entry tree-of-records))) (entry tree-of-records)]
    [(< given-key (key (entry tree-of-records))) (lookup-tree given-key (left-branch tree-of-records))]
    [else (lookup-tree given-key (right-branch tree-of-records))]))


(lookup-tree 6 (tree->2.16.1))
