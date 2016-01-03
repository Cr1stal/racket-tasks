#lang planet neil/sicp

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (inc-by-two x) (+ x 2))

(product-rec cube 1 inc 3)
(product-iter cube 1 inc 3)

(define product product-iter)

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))
(factorial 4)
(factorial 5)

(* 4 (/ (* 2 (product square 4 inc-by-two 110)) (product square 3 inc-by-two 111)))