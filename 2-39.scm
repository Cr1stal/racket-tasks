#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (fold-left op initial (reverse1 sequence)))

(define (reverse1 sequence)
  (if (null? sequence)
      nil
      (append (reverse1 (cdr sequence)) (list (car sequence)))))

(define (reverse sequence)
  (fold-right (lambda (x y) (append x (list y))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse2 (list 1 2 3 4 5))