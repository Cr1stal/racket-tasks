#lang planet neil/sicp

(define (iterative-improve check improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (check guess next)
          next
          (iter next))))
  iter)

(define (close-enough? a b)
  (< (abs (- a b)) 0.00001))

(define (fixed-point f initial-guess)
  ((iterative-improve close-enough? f) initial-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve close-enough? (lambda (y) (average y (/ x y)))) 1.0))

(sqrt 4)
(sqrt 9)
(sqrt 16)