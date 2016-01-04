#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (quadro x) (* x x x x))

((compose square square) 5)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (quadric a b c d e)
  (lambda (x)
    (+ (* a (quadro x)) (* b (cube x)) (* c (square x)) (* d x) e)))

(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(newtons-method (quadric 1 4 -4 -20 -5) 20)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (exp n m)
  (cond ((= m 0) 1)
        ((even? m) (square (exp n (/ m 2))))
        (else (* n (exp n (- m 1))))))

(define (sqrt x n)
  (fixed-point-of-transform (lambda (y) (/ x (exp y (- n 1))))
                            (repeated average-damp n)
                            1.0))


(sqrt 8 3)