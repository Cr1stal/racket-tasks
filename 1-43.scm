#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (reapeted f n)
  (if (= n 0)
      f
      (compose f (reapeted f (- n 1)))))

(define (square x) (* x x))

((reapeted square 2) 5)
