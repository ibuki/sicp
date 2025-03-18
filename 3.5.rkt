#lang racket
(define (real-random range) (* (random) range))
(define (random-in-range low high)
  (let ([range (- high low)]) (+ low (real-random range))))

; (random-in-range 1.0 5.0)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (rand)
  (random 4294967087))
; (estimate-pi 10000000)

(define (square x)
  (* x x))
(define (r x y)
  (sqrt (+ (square x) (square y))))

(define (estimate-integral x1 x2 y1 y2 trials)
  (define (mytest)
    ; (newline)
    ; (display (r (random-in-range x1 x2) (random-in-range y1 y2)))
    (< (r (random-in-range x1 x2) (random-in-range y1 y2)) 1.0))
  (* (- x2 x1) (- y2 y1) (monte-carlo trials mytest)))
; (r -10 10)
; (r 10 10)
; (r 3 4)

(estimate-integral -1.0 1.0 -1.0 1.0 10000000)
(estimate-integral -10.0 10.0 -10.0 10.0 1000000)
(estimate-integral -10.0 10.0 -10.0 10.0 1000000)
; (estimate-integral -10 10 -10 10 10000)
; (estimate-integral -1 1 -1 1 1000)
; (estimate-integral -2 2 -2 2 1000)
; (estimate-integral 0 2 0 2 1000)
