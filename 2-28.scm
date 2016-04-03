#lang planet neil/sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (if (null? x)
      '()
      (if (pair? (car x))
          (append (fringe (car x)) (fringe (cdr x)))
          (append (list (car x)) (fringe (cdr x))))))

(equal? '(1 2 3 4) (fringe x))
(equal? '(1 2 3 4 1 2 3 4) (fringe (list x x)))