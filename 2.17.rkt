#lang racket

(define (last-pair l)
  (let ((next (cdr l)))
  (if (null? next) l (last-pair next))))

(last-pair (list 23 72 149 34))
