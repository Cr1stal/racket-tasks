#lang planet neil/sicp

(define (g y)
  (define (f x)
    (let ((z y))
      (set! y x)
      z))
  f)
(define f (g 0))

(eq? (+ (f 1) (f 0)) 1)

(set! f (g 0))
(eq? (+ (f 0) (f 1)) 0)
