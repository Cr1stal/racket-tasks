#lang planet neil/sicp

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
    (cond ((and (>= lbx 0)
                (>= ubx 0)
                (>= lby 0)
                (>= uby 0))
           ; [ +, + ] x [ +, + ]
           (make-interval (* lbx lby) (* ubx uby)))
           ((and (>= lbx 0)
                (>= ubx 0)
                (<= lby 0)
                (>= uby 0))
           ; [ +, + ] x [ -, + ]
           (make-interval (* ubx lby) (* ubx uby)))
           ((and (>= lbx 0)
                (>= ubx 0)
                (<= lby 0)
                (<= uby 0))
           ; [ +, + ] x [ -, - ]
           (make-interval (* ubx lby) (* lbx uby)))
           ((and (<= lbx 0)
                (>= ubx 0)
                (>= lby 0)
                (>= uby 0))
           ; [ -, + ] x [ +, + ]
           (make-interval (* lbx uby) (* ubx uby)))
           ((and (<= lbx 0)
                (>= ubx 0)
                (<= lby 0)
                (>= uby 0))
           ; [ -, + ] x [ -, + ]
           (make-interval (min (* lbx uby) (* ubx lby))
                          (max (* ubx uby) (* lbx lby))))
           ((and (<= lbx 0)
                (>= ubx 0)
                (<= lby 0)
                (<= uby 0))
           ; [ -, + ] x [ -, - ]
           (make-interval (* ubx lby) (* lbx lby)))
           ((and (<= lbx 0)
                (<= ubx 0)
                (>= lby 0)
                (>= uby 0))
           ; [ -, - ] x [ +, + ]
           (make-interval (* lbx uby) (* ubx lby)))
           ((and (<= lbx 0)
                (<= ubx 0)
                (<= lby 0)
                (>= uby 0))
           ; [ -, - ] x [ -, + ]
           (make-interval (* lbx uby) (* lbx lby)))
           ((and (<= lbx 0)
                (<= ubx 0)
                (<= lby 0)
                (<= uby 0))
           ; [ -, - ] x [ -, - ]
           (make-interval (* ubx uby) (* lbx lby))))))

(define (div-interval x y)
  (if (or (= 0 (lower-bound y)) (= 0 (upper-bound y)))
      (error "Upper or lower bound of y is zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(display (mul-interval (make-interval -1 5) (make-interval 3 4)))
