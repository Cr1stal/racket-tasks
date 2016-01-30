#lang planet neil/sicp

(define (make-segment x y)
  (cons x y))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((sp (start-segment s)) (ep (end-segment s)))
       (make-point (/ (+ (x-point sp) (x-point ep)) 2)
                   (/ (+ (y-point sp) (y-point ep)) 2))))
                   

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 1 2))
(define p2 (make-point 10 2))
(define p3 (make-point 1 10))
(define p4 (make-point 10 10))

(print-point (midpoint-segment s))
