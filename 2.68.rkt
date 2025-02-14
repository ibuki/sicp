#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (cond
    [(= bit 0) (left-branch branch)]
    [(= bit 1) (right-branch branch)]
    [else (error "bad bit: CHOOSE-BRANCH " bit)]))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
(define (make-code-tree-from-leaf-set leaf-set)
  (if (null? (cddr leaf-set))
      (make-code-tree (car leaf-set) (cadr leaf-set))
      (make-code-tree (car leaf-set) (make-code-tree-from-leaf-set (cdr leaf-set)))))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(define sample-tree2 (make-code-tree-from-leaf-set (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

sample-tree
sample-tree2
(decode sample-message sample-tree)
(decode sample-message sample-tree2)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)

  (define (encode-symbol-1 bits current-tree)
    (cond ((equal? (list symbol) (symbols (left-branch current-tree))) (append bits '(0)))
          ((equal? (list symbol) (symbols (right-branch current-tree))) (append bits '(1)))
          (else (encode-symbol-1 (append bits '(1)) (right-branch current-tree)))))
  (encode-symbol-1 '() tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

; (equal? '(A) '(A B))
; (append '() '(0))
; (encode-symbol '() sample-tree)
(encode '(A) sample-tree)
(encode '(A B C D) sample-tree)
