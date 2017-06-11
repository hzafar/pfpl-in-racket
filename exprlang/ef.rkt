#lang turnstile/lang
(extends "e.rkt" #:except #%datum)

(require
  "../helpers.rkt"
  "ef-dynamics.rkt")

(provide #%datum #%app λ :ef)

;; Types
(define-type-constructor → #:arity = 2)

;; Statics
(define-typed-syntax #%datum
  [(_ . n:number) ≫
   --------
   [⊢ (#%datum- . n) ⇒ num]]
  [(_ . s:str) ≫
   --------
   [⊢ (#%datum- . s) ⇒ str]])
  
(define-typed-syntax (λ (x:id : T_1:type) e) ≫
  [[x ≫ x- : T_1] ⊢ e ≫ e- ⇒ T_2]
  --------
  [⊢ (#%app- list 'lam (qo  x-) (qo e-)) ⇒ (→ T_1 T_2)])

(define-typed-syntax (#%app e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ (~→ T_2 T)]
  [⊢ e2 ≫ e2- ⇐ T_2]
  --------
  [⊢ (#%app- list 'ap (qo e1-) (qo e2-)) ⇒ T])

(define-syntax-rule (:ef e)
  (#%app- evaluate e))