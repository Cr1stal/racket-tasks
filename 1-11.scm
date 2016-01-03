#lang planet neil/sicp

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (f-rec (- n 2)) (f-rec (- n 3)))))

(f-rec 4)
(f-rec 5)
(f-rec 6)
(f-rec 10)

(define (f-iter n)
  (define (iter c a1 a2 a3)
    (if (eq? c n)
        a1
        (iter (+ c 1) (+ a1 a2 a3) a1 a2)))
  (iter 2 2 1 0))

(f-iter 4)
(f-iter 5)
(f-iter 6)
(f-iter 10)
