#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node) (if (pair? node) (count-leaves node) 1)) t)))

(count-leaves (list 1 (list 1 2 3) 0 (list 1 1) 0 1))
