#lang planet neil/sicp

(define (random)
  (define x 1232423)

  (define (rand-update x)
    (modulo (+ (* 12412342 x) 23235234) 2323235234))
  (define generate
      (lambda ()
        (set! x (rand-update x))
        x))
  (define (reset n) (set! x n))

  (define (dispatch m)
    (cond ((eq? m 'generate) (generate))
          ((eq? m 'reset) reset)
          (else (error "Неизвестный вызов -- RANDOM"
                       m))))
  dispatch)
