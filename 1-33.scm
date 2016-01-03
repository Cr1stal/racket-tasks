#lang planet neil/sicp

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner filter null-value term (next a) next b))
          (filtered-accumulate combiner filter null-value term (next a) next b)
)))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (sum-prime-squares a b)
  (filtered-accumulate (lambda(r s)(+ r s)) (lambda(r)(prime? r)) 0 square a inc b))
(sum-prime-squares 1 5)
(define (identity x) x)
(define (product-prime n)
  (filtered-accumulate (lambda(r s)(* r s)) (lambda(r)(= 1 (gcd r n))) 1 identity 1 inc n))

(product-prime 10)
