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

(define s (make-segment p1 p2))
(print-point (midpoint-segment s))

(define (make-rectangle ts bs ls rs)
  (cons ts (cons bs (cons ls (cons rs '())))))

(define (top-line r)
  (car r))

(define (bottom-line r)
  (cadr r))

(define (left-line r)
  (caddr r))

(define (right-line r)
  (cadddr r))

(define (square x)
  (* x x))

(define (line-length line)
  (let ((sp (start-segment line)) (ep (end-segment line)))
    (sqrt (+ (square (- (x-point ep) (x-point sp))) (square (- (y-point ep) (y-point sp)))))))

(define (perimeter-rec r)
  (+ (line-length (top-line r)) (line-length (bottom-line r)) (line-length (left-line r)) (line-length (right-line r))))

(define (square-rec r)
  (* (line-length (top-line r)) (line-length (left-line r))))

(define tl (make-segment p1 p2))
(define bl (make-segment p3 p4))
(define ll (make-segment p1 p3))
(define rl (make-segment p2 p4))

(define r (make-rectangle tl bl ll rl))

(display (perimeter-rec r))