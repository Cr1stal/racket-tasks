#lang planet neil/sicp

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (square-tree-iter tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-iter (car tree))
                    (square-tree-iter (cdr tree))))))

(equal? '(1 (4 (9 16) 25) (36 49))
        (square-tree-iter
         (list 1
               (list 2 (list 3 4) 5)
               (list 6 7))))
