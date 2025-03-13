#lang racket
; (define (make-account balance)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin
;           (set! balance (- balance amount))
;           balance)
;         " Insufficient funds "))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch m)
;     (cond
;       [(eq? m 'withdraw) withdraw]
;       [(eq? m 'deposit) deposit]
;       [else (error " Unknown request : MAKE-ACCOUNT " m)]))
;   dispatch)
; (define acc (make-account 100))

; ((acc 'withdraw) 50)
; ((acc 'withdraw) 50)

(define (make-accumulator balance)
  (lambda (amount)
    (set! balance (+ balance amount))
    balance))

(define A (make-accumulator 5))
(A 10)
(A 10)
