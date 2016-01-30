#lang planet neil/sicp

(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

(define (asdf n base)
  (define (inner n k)
    (if (= (modulo n base) 0)
        (inner (/ n base) (+ k 1))
        k))
  (inner n 0))
      

(define (cons x y)
  (* (pow 2 x) (pow 3 y)))

(define (car z)
  (asdf z 2))

(define (cdr z)
  (asdf z 3))

(define z (cons 3 4))
(display (car z))
(display (cdr z))