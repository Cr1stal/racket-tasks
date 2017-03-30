#lang planet neil/sicp

(define (filter pred seq)
  (if (null? seq)
      nil
      (if (pred (car seq))
          (cons (car seq) (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))
  
(define (adjoin-position new-row k rest-of-queens)
  (append (list (cons k new-row)) rest-of-queens))

(define (safe? k positions)
  (let ((target (car positions)))
    (if (null? target)
        false
        (= 0 (length (filter (lambda (position)
                               (or
                                (= (abs (- (car target) (car position)))
                                   (abs (- (cdr target) (cdr position))))
                                (= (cdr target) (cdr position))
                                (= (car target) (car position))
                                )
                               )
                             (cdr positions)))))))

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 7))