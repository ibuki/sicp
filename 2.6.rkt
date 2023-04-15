(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)

;;; (define one
;;;   (add-1 zero))

;;; (add-1 zero)
;;; ((lambda (f) (lambda (x) (f ((n f) x)))) (lambda (f) (lambda (x) x)))
;;; (lambda (x) ((lambda (f) (lambda (x2) x2)) ((n (lambda (f) (lambda (x2) x2))) x)))
;;; (lambda (x) ((lambda (f) (lambda (x2) x2)) ((n (lambda (f) (lambda (x2) x2))) x)))

(add-1 zero)
((lambda (f) (lambda (x) (f ((zero f) x)))))
((lambda (g) (lambda (y) (g ((zero g) y)))))
((lambda (g) (lambda (y) (g (((lambda (f) (lambda (x) x)) g) y)))))
((lambda (g) (lambda (y) (g y))))
(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
((lambda (f) (lambda (x) (f ((one f) x)))))
((lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x)))))
((lambda (f) (lambda (x) (f ((lambda (y) (f y)) x)))))
((lambda (f) (lambda (x) (f (f x)))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
  ()
)


(+ one two)
((lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(one one)
((lambda (f) (lambda (x) (f x))) one)
((lambda (x) (one x)))
((lambda (x) ((lambda (f) (lambda (y) (f y))) x)))
((lambda (f) (lambda (x) (f x))))

(one two)
((lambda (f) (lambda (x) (f x))) two)
((lambda (x) (two x)))
((lambda (x) ((lambda (g) (lambda (y) (g (g y)))) x)))
((lambda (x) ((lambda (y) (x (x y))))))
((lambda (f) (lambda (x) (f (f x)))))

(two one)
((lambda (f) (lambda (x) (f (f x)))) one)
((lambda (x) (one (one x))))
((lambda (x) (one ((lambda (g) (lambda (y) (g y))) x))))
((lambda (x) (one (lambda (y) (x y)))))
((lambda (x) ((lambda (g) (lambda (z) (g z))) (lambda (y) (x y)))))
((lambda (x) (lambda (z) ((lambda (y) (x y)) z))))
((lambda (f) (lambda (x) (f x))))

(define (add-2 n)
  (lambda (f) (lambda (x) (f (f ((n f) x))))))
