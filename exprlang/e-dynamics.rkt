#lang racket

(require "../helpers.rkt")
(provide evaluate)

(define (val? e)
  (or (number? e) (string? e)))

;; E Dynamics, following PFPL 5.4a-h.
(define (evaluate e)
  (match e
    [(? val? v) v]
    [`(+ ,(? val? e1) ,(? val? e2)) (+ e1 e2)]
    [`(+ ,(? val? e1) ,e2) (evaluate `(+ ,e1 ,(evaluate e2)))]
    [`(+ ,e1 ,e2) (evaluate `(+ ,(evaluate e1) ,e2))]
    [`(* ,(? val? e1) ,(? val? e2)) (* e1 e2)]
    [`(* ,(? val? e1) ,e2) (evaluate `(* ,e1 ,(evaluate e2)))]
    [`(* ,e1 ,e2) (evaluate `(* ,(evaluate e1) ,e2))]
    [`(^ ,(? val? s1) ,(? val? s2)) (string-append s1 s2)]
    [`(^ ,(? val? s1) ,s2) (evaluate `(^ ,s1 ,(evaluate s2)))]
    [`(^ ,s1 ,s2) (evaluate `(^ ,(evaluate s1) ,s2))]
    [`(len ,(? val? s)) (string-length s)]
    [`(len ,s) (evaluate `(len ,(evaluate s)))]
    [`(let ,x ,(? val? e1) ,e2) (evaluate (substitute e1 x e2))]
    [`(let ,x ,e1 ,e2) (evaluate `(let ,x ,(evaluate e1) ,e2))]))