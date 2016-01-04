#lang planet neil/sicp

(define dx 0.000001)

(define (average3 x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      f
      (compose f (repeated f (- n 1)))))

(define (smoothN f n)
  (repeated (smooth f) n))

((smoothN (lambda (x) (inc x)) 8) 4)