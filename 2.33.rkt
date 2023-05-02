#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (square x)
  (* x x))
(define lst1 (list 1 2 3 4))
(define lst2 (list 4 5 6 7))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(map square lst1)
(append lst1 lst2)
(length lst2)
