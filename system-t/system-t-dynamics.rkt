#lang racket

(require "../helpers.rkt")
(provide evaluate to-number)

(define (z? e) (and (number? e) (zero? e)))

;; A closed value is either 0, a successor expression where the
;; argument is a closed value, or a lambda expression.
(define (val? e)
  (match e
    [(or (? z? _)
         `(succ ,(? val? _))
         `(lam ,_ ,_)) #t]
    [_ #f]))

;; The evaluator steps over the expression and proceeds according to the type,
;; following the dynamics given in PFPL 9.2a-g.
(define (evaluate e)
  (match e
    [(? val? e) e]
    [`(succ ,expr) (evaluate `(succ ,(evaluate expr)))]
    [`(ap (lam ,x ,body) ,(? val? arg)) (evaluate (substitute arg x body))]
    [`(ap ,(? val? fn) ,arg) (evaluate `(ap ,fn ,(evaluate arg)))]
    [`(ap ,fn ,arg) (evaluate `(ap ,(evaluate fn) ,arg))]
    [`(rec ,(? z? e) ,e0 ,x ,y ,e1) (evaluate e0)]
    [`(rec ,(? val? (list 'succ expr)) ,e0 ,x ,y ,e1)
     (evaluate (substitute `(rec ,expr ,e0 ,x ,y ,e1) y (substitute expr x e1)))]
    [`(rec ,e ,e0 ,x ,y ,e1) (evaluate `(rec ,(evaluate e) ,e0 ,x ,y ,e1))]
    [_ (error "Unrecognized expression!" e)]))

(define (to-number e)
  (match e
    [(? z? _) 0]
    [`(succ ,expr) (add1 (to-number expr))]))
