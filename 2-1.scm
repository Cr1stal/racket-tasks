#lang planet neil/sicp

(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and (< n 0) (< d 0))
        (cons (/ (abs n) g) (/ (abs d) g))
        (if (< d 0)
            (cons (/ (- 0 n) g) (/ (abs d) g))
            (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define r1 (make-rat 3 -4))
(define r2 (make-rat -1 2))

(print-rat (add-rat r1 r2))