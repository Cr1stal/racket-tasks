#lang planet neil/sicp

(define x (list 1 2 3))
(define y (list 4 5 6))

(equal? '(1 2 3 4 5 6) (append x y))
(equal? (cons '(1 2 3) '(4 5 6)) (cons x y))
(equal? '((1 2 3) (4 5 6)) (list x y))
