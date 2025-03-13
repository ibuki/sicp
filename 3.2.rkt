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

(define (make-monitored f)
  (define count 0)
  (lambda (x)
    (cond
    ((eq? x 'how-many-calls?) count)
    ((eq? x 'reset-count) (set! count 0))
    (else (begin (set! count (+ count 1)) (f x))))))

(define s ( make-monitored sqrt ))
(s 100)
(s 'how-many-calls? )
(s 100)
(s 'how-many-calls? )
(s 'how-many-calls? )
(s 'reset-count )
(s 'how-many-calls? )
