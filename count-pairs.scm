#lang planet neil/sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 'a 'b 'c))
(count-pairs (list 'a 'b 'c 'd))
(count-pairs (list 'a 'b 'c 'd 'e 'f 'g))

(member 'b (list 'a 'b 'c 'd))
(define x (list 'a 'b 'c))
(define z1 (cons x x))

(define (count-pairs-right1 x)
  (define (iter y s)
    (if (null? y)
        0
        (if (member (car y) s)
            0
            (+ 1 (iter (cdr y) (cons (car y) s))))))
  (iter x '()))

(define (count-pairs-right x) 
  (let ((encountered '())) 
    (define (helper x) 
      (if (or (not (pair? x)) (memq x encountered)) 
          0 
          (begin 
            (set! encountered (cons x encountered)) 
            (+ (helper (car x)) 
               (helper (cdr x)) 
               1)))) 
    (helper x))) 

(count-pairs-right (list 'a 'b 'c))
(count-pairs-right z1)

(count-pairs-right1 (list 'a 'b 'c))
(count-pairs-right1 z1)

(define u (cons 1 2))
(define z (cons u u))

u
z
(define b (list 'a 'b)) 
(set-cdr! (cdr b) (cdr b))
b