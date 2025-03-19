#lang racket
(define (show-error x) (display 'show-error) (display x))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        " Insufficient funds "))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (show-error amount)
    "Incorrect password")
  (define (dispatch try-password m)
    (cond
      [(not (eq? password try-password)) show-error]
      [(eq? m 'withdraw) withdraw]
      [(eq? m 'deposit) deposit]
      [else (error " Unknown request : MAKE-ACCOUNT " m)]))
  dispatch)
; (define acc (make-account 100))

; (define acc (make-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 40)
; ((acc 'some-other-password 'deposit) 50)

(define (make-joint acc pw1 pw2)
  (define (dispatch try-password m)
    (cond
      [(not (eq? pw2 try-password)) show-error]
      [else (acc pw1 m)]))
  dispatch)

(define peter-acc (make-account 200 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 40)
