#lang racket
(define current 1)
(define (rand key)
  (cond
    [(eq? key 'generate)
     (begin
       (set! current (+ current 1))
       current)]
    [(eq? key 'reset) (lambda (new-value) (set! current new-value))]))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 2)
(rand 'generate)
(rand 'generate)
(rand 'generate)
