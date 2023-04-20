#lang racket

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;;; (define (make-mobile left right) (cons left right))
;;; (define (make-branch length structure) (cons length structure))
;;; (define (left-branch mobile)
;;;   (car mobile))
;;; (define (right-branch mobile)
;;;   (cdr mobile))
;;; (define (branch-length branch)
;;;   (car branch))
;;; (define (branch-structure branch)
;;;   (cdr branch))


(define (total-weight mobile)
  (define (branch-weight branch)
    (let ([structure (branch-structure branch)])
      (if (pair? structure) (total-weight structure) structure)))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define mobile1 (make-mobile (make-branch 6 2) (make-branch 3 4)))
(define mobile2 (make-mobile (make-branch 2 mobile1) (make-branch 3 4)))

(total-weight mobile1)
(total-weight mobile2)

(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (let ([structure (branch-structure branch)] [length (branch-length branch)])
      (* length (if (pair? structure) (total-weight structure) structure))))
  (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile))))

(mobile-balanced? mobile1)
(mobile-balanced? mobile2)
