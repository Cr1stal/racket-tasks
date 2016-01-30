#lang planet neil/sicp

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define four (lambda (f) (lambda (x) (f (f (f (f x)))))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (plus k l)
  (lambda (f) (lambda (x) ((l f) ((k f) x)))))

(define (inc n)
  (+ n 1))


(display ((zero inc) 0))
(newline)
(display ((one inc) 0))
(newline)
(display ((two inc) 0))
(newline)
(display (((add-1 zero) inc) 0))
(newline)
(define seven (plus three four))
(display (((plus seven three) inc) 0))