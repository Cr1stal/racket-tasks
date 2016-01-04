#lang planet neil/sicp

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
  (lambda(x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (fixed-point g first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (try guess)
    (let ((next (g guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(newtons-method (cubic 3 9 14) 1)