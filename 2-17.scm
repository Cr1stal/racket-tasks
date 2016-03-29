#lang planet neil/sicp

(define (last-pair xs)
  (if (null? xs)
      '()
      (if (null? (cdr xs))
          (car xs)
          (last-pair (cdr xs)))))

(last-pair (list 23 72 149 34))
