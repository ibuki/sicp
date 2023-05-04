#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs)) (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 1 2 3))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector (list (list 2 3 4) (list 3 4 5)) (list 1 2 3))

(define (transpose mat)
  (accumulate-n cons null mat))

(define m1 (list (list 2 3 4) (list 1 2 3)))
(define m2 (transpose m1))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)]) (map (lambda (v) (map (lambda (w) (dot-product v w)) cols)) m)))

(matrix-*-matrix m1 m2)
