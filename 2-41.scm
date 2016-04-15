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

(define (filter pred seq)
  (if (null? seq)
      nil
      (if (pred (car seq))
          (cons (car seq)
                (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

(define (solution n s)
  (filter (lambda (triple)
            (= s (+ (car triple) (cadr triple) (caddr triple))))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 n)))
                              (enumerate-interval 1 n)))
                   (enumerate-interval 1 n))))

(solution 5 10)
