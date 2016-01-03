#lang planet neil/sicp

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fast-mul a b)
  (define (iter a b s)
    (cond ((= b 0) s)
          ((even? b)
           (iter (double a) (halve b) s))
          (else
           (iter a (- b 1) (+ a s)))))
  (iter a b 0))

(fast-mul 3 5)
