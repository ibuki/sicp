;; uses get/put (from 3.3.3) -- see ch2support.scm

; ch2support.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;-----------
;;;from section 3.3.3 for section 2.4.3
;;; to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;-----------




; and finally some testing code from me...
; (install-rectangular-package) ; don't forget! otherwise "the object #f is not applicable"
; (install-polar-package)

; ; the resulting operations are pretty easy for the end-user, though
; (define z1 (make-from-real-imag 1 1))
; (define z2 (make-from-mag-ang 2 (acos -1)))
; (newline) (display (magnitude z1))
; (newline) (display (imag-part z2))

; (newline) (display (map type-tag (list z1)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

( define ( variable? x) ( symbol? x))
( define ( same-variable? v1 v2) (and ( variable? v1) ( variable? v2) (eq? v1 v2 )))
; ( define ( make-sum a1 a2) ( list '+ a1 a2 ))
; ( define ( make-product m1 m2) ( list '* m1 m2 ))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

;;; (define (make-product m1 m2)
;;;   (list '* m1 m2))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))
( define ( sum? x) (and ( pair? x) (eq? (car x) ' +)))
( define ( addend s) ( cadr s))
( define ( augend s) ( caddr s))
( define ( product? x) (and ( pair? x) (eq? (car x) '* )))
( define ( multiplier p) ( cadr p))
( define ( multiplicand p) ( caddr p))
(define (make-exponentiation base exponent)
  (cond
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent)) (exp base exponent))
    (else (list '** base exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp ))
               (operands exp) var ))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  (put 'deriv '+
    (lambda (operands var) (make-sum (deriv (car operands) var) (deriv (cadr operands) var))))
  (put 'deriv '*
    (lambda (operands var) (make-sum (make-product (car operands) (deriv (cadr operands) var)) (make-product (deriv (car operands) var) (cadr operands)))))
  (put 'deriv '**
    (lambda (operands var) (make-product (cadr operands) (make-product (make-exponentiation (car operands) (- (cadr operands) 1)) (deriv (car operands) var)))))
  'done)

; (define (exponentiation? x)
;   (and (pair? x) (eq? (car x) '**)))
; ((exponentiation? exp)
;  (make-product (exponent exp)
;                (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
;                              (deriv (base exp) var))))

(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))
(install-deriv-package)

(define exp1 '(+ x (* x y)))
(newline) (display (deriv exp1 'x))

(define exp2 '(+ (** x 3) (** x 2)))
(newline) (display (deriv exp2 'x))
