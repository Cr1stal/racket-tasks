#lang planet neil/sicp

(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (tree-map1 func tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map func (car tree))
                    (tree-map func (cdr tree))))))
(define (square x)
  (* x x))

(define (square-tree tree) (tree-map square tree))

(equal? '(1 (4 (9 16) 25) (36 49))
        (square-tree
         (list 1
               (list 2 (list 3 4) 5)
               (list 6 7))))
