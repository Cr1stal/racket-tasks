#lang planet neil/sicp

(define (make-account-secret balance secret)
  (define (withdraw amount)
   (if (>= balance amount)
    (begin (set! balance (- balance amount))
     balance)
    "Недостаточно денег на счете"))

 (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

   (define (call-the-cops) "Мы позвонили в полицию")

 (let ((calls 0))
   (lambda (password m)
    (if (eq? password secret)
     (begin
      (set! calls 0)
      (cond ((eq? m 'withdraw) withdraw)
       ((eq? m 'deposit) deposit)
       (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
              m))))
     (lambda (m)
      (if (>= calls 7)
       (call-the-cops)
       (begin
        (set! calls (+ calls 1))
        "Неверный пароль"
       )
      )
     )
    )
)
 )
)

(define acc (make-account-secret 100 'secret-password))
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
((acc 'secret-other-password 'withdraw) 50)
