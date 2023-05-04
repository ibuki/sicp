#lang racket
(define nil null)
(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high) null (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (include? value values)
  (cond
    [(null? values) false]
    [(= value (car values)) true]
    [else (include? value (cdr values))]))

(define (fn-include? fn value values)
  (include? (fn value) (map fn values)))

(define (has-same-row? position positions)
  (fn-include? cadr position positions))

(define (sum position)
  (+ (car position) (cadr position)))

(define (abs-sub position)
  (- (car position) (cadr position)))

(define (has-same-naname? position positions)
  (or (fn-include? sum position positions) (fn-include? abs-sub position positions)))

(define (safe? positions)
  (not (or (has-same-row? (car positions) (cdr positions))
      (has-same-naname? (car positions) (cdr positions)))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))
(define empty-board null)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
