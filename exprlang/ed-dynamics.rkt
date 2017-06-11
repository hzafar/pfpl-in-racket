#lang racket

(require "../helpers.rkt")
(provide evaluate)

(define (val? e)
  (or (number? e) (string? e)))

;; ED Dynamics, following PFPL 8.3.
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
    [`(let ,x ,e1 ,e2) (evaluate `(let ,x ,(evaluate e1) ,e2))]
    [`(fun ,f ,x1 ,e2 ,e) (evaluate (fn-substitute f x1 e2 e))]))

;; Function substitution as per 8.2.
(define (fn-substitute f arg body expr)
  (cond [(empty? expr) empty]
        [(and (cons? expr) (equal? (car expr) 'apply)) `(let ,arg ,(fn-substitute f arg body (caddr expr)) ,body)]
        [(cons? expr) (cons (fn-substitute f arg body (car expr)) (fn-substitute f arg body (cdr expr)))]
        [else expr]))