#lang planet neil/sicp

(define x (list (list 1 2) (list 3 4)))

(define (reverse x)
  (if (null? x)
      '()
      (append (reverse (cdr x)) (list (car x)))))

(equal? '((3 4) (1 2)) (reverse x))

(define (deep-reverse x)
  (if (null? x)
      '()
      (if (pair? (car x))
          (append (deep-reverse (cdr x)) (list (deep-reverse (car x))))
          (append (deep-reverse (cdr x)) (list (car x))))))

(equal? '((4 3) (2 1)) (deep-reverse x))
