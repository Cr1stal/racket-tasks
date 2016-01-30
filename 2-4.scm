#lang planet neil/sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car x)
  (z (lambda (p q) p)))

(define (cdr x)
  (z (lambda (p q) q)))

(define z (cons 1 2))
(display (car z))
(newline)
(display (cdr z))