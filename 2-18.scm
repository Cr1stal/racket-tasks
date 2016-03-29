#lang planet neil/sicp

(define (reverse xs)
  (if (null? (cdr xs))
      (list (car xs))
      (append (reverse (cdr xs)) (list (car xs)))))

(reverse (list 1 4 9 16 25))
