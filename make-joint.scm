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

  (define (is-valid-password pass) (eq? pass secret))


 (let ((calls 0))
   (define (with-count func)
     (begin
      (set! calls 0)
     func))
     
  (define (with-secret-check password func)
    (if (eq? password secret)
        (with-count func)
     (lambda (m)
      (if (>= calls 7)
       (call-the-cops)
       (begin
        (set! calls (+ calls 1))
        "Неверный пароль"
       )
      )
     )))
    
   (lambda (password m)
      (cond
        ((eq? m 'withdraw) (with-secret-check password withdraw))
        ((eq? m 'deposit) (with-secret-check password deposit))
        ((eq? m 'is-valid-password) is-valid-password)
        (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
              m))))
    )
)

(define (make-joint account password-on-create password-on-use)
  (if ((account '- 'is-valid-password) password-on-create)
      (lambda(password op)
        (if (eq? password password-on-use)
            (account password-on-create op)
            (lambda(v) "Hеверный пароль"))
        )
  (error "Неверный пароль от текущего аккаунта")))

(define peter-acc
  (make-account-secret 100 'open-sesame))

(define paul-acc2
  (make-joint peter-acc 'open+sesame 'rosebud))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'secret 'deposit) 100)
((paul-acc2 'secret 'deposit) 100)