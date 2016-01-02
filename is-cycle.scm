#lang planet neil/sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(cddddr z)

(define (is-cycle x)
  (define (iter x initial)
    (if (null? x)
        false
        (if (eq? (car x) initial)
            true
            (iter (cdr x) initial))))
  (iter (cdr x) (car x)))          


(is-cycle z)
(is-cycle (list 'a 'b 'c))