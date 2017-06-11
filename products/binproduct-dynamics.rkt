#lang racket

(provide evaluate)

;; Dynamics, following PFPL 10.2a-h.
(define (val? e)
  (match e
    ['null #t]
    [`(pair ,(? val? e1) ,(? val? e2)) #t]
    [_ #f]))

(define (evaluate e)
  (match e
    [(? val? e) e]
    [`(pair ,(? val? e1) ,e2) (evaluate `(pair ,e1 ,(evaluate e2)))]
    [`(pair ,e1 ,e2) (evaluate `(pair ,(evaluate e1) ,e2))]
    [`(left (pair ,(? val? e1) ,(? val? e2))) e1]
    [`(left ,e) (evaluate `(left ,(evaluate e)))]
    [`(right (pair ,(? val? e1) ,(? val? e2))) e2]
    [`(right ,e) (evaluate `(right ,(evaluate e)))]
    [_ (error "Unable to evaluate:" e)]))



