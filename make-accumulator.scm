#lang planet neil/sicp

(define (make-accumulator start)
  (let ((sum start))
  (lambda (amount)
    (set! sum (+ sum amount))
    sum
    )))

