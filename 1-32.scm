#lang planet neil/sicp

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define accumulate accumulate-iter)

(define (sum term a next b)
  (accumulate (lambda(r s) (+ r s)) 0 term a next b))
(define (product term a next b)
  (accumulate (lambda(r s) (* r s)) 1 term a next b))

(define (inc x) (+ x 1))
(define (square x) (* x x))

(sum square 1 inc 3)
(product square 1 inc 3)
