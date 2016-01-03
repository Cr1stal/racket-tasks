#lang planet neil/sicp

(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (* (/ h 3)
     (+ (f a)
        (sum (lambda (r) (* 4 (f (+ a (* r h))))) 1 (lambda (r) (+ r 2)) (- n 1))
        (sum (lambda (r) (* 2 (f (+ a (* r h))))) 2 (lambda (r) (+ r 2)) (- n 2))
        (f (+ a (* h n))))))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simpson cube 0 1 100)