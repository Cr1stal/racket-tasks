#lang planet neil/sicp

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(enumerate-interval 1 10)

(unique-pairs 5)

(define (make-sum-pair pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter pred seq)
  (if (null? seq)
      nil
      (if (pred (car seq))
          (cons (car seq)
                (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

(define (prime? n)
  (define (square x) (* x x))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-sum-pair
       (filter prime-sum?
               (unique-pairs n))))
  

(prime-sum-pairs 5)