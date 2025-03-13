#lang racket
(define (call-the-cops) "cops here")

(define (make-account balance password)
  (define count 0)
  (define (withdraw amount)
    (set! count 0)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        " Insufficient funds "))
  (define (deposit amount)
    (set! count 0)
    (set! balance (+ balance amount))
    balance)
  (define (show-error amount)
    (begin
      (set! count (+ count 1))
      (if (> count 6) (call-the-cops) "Incorrect password")
      ))
  (define (dispatch try-password m)
    (cond
      [(not (eq? password try-password)) show-error]
      [(eq? m 'withdraw) withdraw]
      [(eq? m 'deposit) deposit]
      [else (error " Unknown request : MAKE-ACCOUNT " m)]))
  dispatch)
; (define acc (make-account 100))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
