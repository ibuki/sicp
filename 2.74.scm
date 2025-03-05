;; uses get/put (from 3.3.3) -- see ch2support.scm

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; Generic selectors

(define (get-record records key) (apply-generic 'get-record records key))
(define (get-salary records key) (apply-generic 'get-salary records key))
;;;-----------

(define (install-office1-package)
  ;; internal procedures
  (define (get-record records key)
    (cond ((null? records) 'empty)
          ((eq? (caar records) (car key)) (car records))
          (else (get-record (cdr records) key))))

  (define (get-salary records key)
    (cadr (get-record records key)))

  ;; interface to the rest of the system
  ; (define (tag x) (attach-tag 'office1 x))
  (put 'get-record '(office1 name) get-record)
  (put 'get-salary '(office1 name) get-salary)
  'done)

(install-office1-package)

(define (install-office2-package)
  ;; internal procedures
  (define (get-record records key)
    (cond ((null? records) 'empty)
          ((eq? (caar records) (car key)) (car records))
          (else (get-record (cdr records) key))))

  (define (get-salary records key)
    (caddr (get-record records key)))

  ;; interface to the rest of the system
  ; (define (tag x) (attach-tag 'office1 x))
  (put 'get-record '(office2 name) get-record)
  (put 'get-salary '(office2 name) get-salary)
  'done)

(install-office2-package)

; (define exp1 '(+ x (* x y)))
; (newline) (display (deriv exp1 'x))

; (define exp2 '(+ (** x 3) (** x 2)))
; (newline) (display (deriv exp2 'x))

(define office1-file '(office1 (name1 salary1) (name2 salary2)))
(define office2-file '(office2 (name3 address3 salary3) (name4 address4 salary4)) )
(newline) (display (get-record office1-file '(name name1)))
(newline) (display (get-record office1-file '(name name4)))
(newline) (display (get-record office2-file '(name name4)))
(newline) (display (get-salary office1-file '(name name1)))
(newline) (display (get-salary office2-file '(name name4)))

(define files (list office1-file office2-file))
(define (find-employee-record files key)
  (if (null? files) 'empty
    (let ((result (get-record (car files) key)))
      (if (eq? result 'empty)
        (find-employee-record (cdr files) key) result))))
(newline) (display (find-employee-record files '(name name2)))
(newline) (display (find-employee-record files '(name name4)))
