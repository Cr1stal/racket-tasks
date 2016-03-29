#lang planet neil/sicp

(define (odd? x)
  (= (remainder x 2) 1))

(define (even? x)
  (= (remainder x 2) 0))

(define (filter xs pred)
  (if (null? xs)
      '()
      (if (pred (car xs))
          (cons (car xs) (filter (cdr xs) pred))
          (filter (cdr xs) pred))))

(define (same-parity . xs)
  (if (null? xs)
      '()
      (if (odd? (car xs))
          (filter xs odd?)
          (filter xs even?))))

(filter (list 1 2 3 4) odd?)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
