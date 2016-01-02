#lang planet neil/sicp

(define (make-monitored func)
  (let ((calls 0))
   (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) calls)
          (else
           (set! calls (+ calls 1))
           (func m)
           )
          ))
  dispatch))
