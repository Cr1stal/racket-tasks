#lang planet neil/sicp

(define (square x)
  (* x x))

(define (square-list-rec items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-rec (cdr items)))))

(square-list-rec (list 1 2 3 4 5))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4 5))