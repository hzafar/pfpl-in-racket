#lang racket

(require "../helpers.rkt")
(provide evaluate)

(define (val? e)
  (or (number? e) (string? e)
      (and (cons? e) (equal? (car e) 'lam))))

;; EF Dynamics, following PFPL 8.5b-d. Call-by-value dynamics given below;
;; comment out the starred lines and uncomment the last case for the
;; call-by-name interpretation.
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
    [`(ap (lam ,x ,e) ,(? val? e2)) (evaluate (substitute e2 x e))] ;; ****
    [`(ap ,(? val? e1) ,e2) (evaluate `(ap ,e1 ,(evaluate e2)))] ;; ****
    [`(ap ,e1 ,e2) (evaluate `(ap ,(evaluate e1) ,e2))]
    ;[`(ap (lam ,x ,e) ,e2) (evaluate (substitute ,e2 ,x ,e))]
    ))