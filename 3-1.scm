#lang planet neil/sicp

(define (make-account-secret balance secret)
 (let ((calls 0))
  (define (withdraw amount)
   (if (>= balance amount)
    (begin (set! balance (- balance amount))
     balance)
    "Недостаточно денег на счете"))
 )

 (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

   (define (call-the-cops) "Мы позвонили в полицию")

   (define (dispatch password m)
    (if (eq? password secret)
     (begin
      (set! calls 0)
      (cond ((eq? m 'withdraw) withdraw)
       ((eq? m 'deposit) deposit)
       (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
              m))))
     (lambda (m)
      (if (> calls 7)
       (call-the-cops)
       (begin
        (set! calls calls + 1)
        "Неверный пароль"
       )
      )
     )
    )
    dispatch)
)

(define acc (make-account-secret 100 'secret-password))
((acc 'secret-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-password 'withdraw) 30)
