#lang planet neil/sicp

(define (pascal c r)
  (cond ((= c 0) 1)
        ((= c r) 1)
        (else
         (+ (pascal (- c 1) (- r 1)) (pascal c (- r 1))))))

(pascal 1 3)
(pascal 2 3)
(pascal 2 4)
(pascal 1 4)
(pascal 3 4)
(pascal 2 5)
