#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (square x) (* x x))

(map square (list 1 2 3 4 5))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (map (lambda (t) (dot-product w t)) cols)) m)))

(define r (list (list 1 3 2) (list 0 4 -1)))
(define k (list (list 2 0 -1 1) (list 3 -2 1 2) (list 0 1 2 3)))

(matrix-*-matrix r k)