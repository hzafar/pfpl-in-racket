#lang turnstile/lang
(extends "e.rkt" #:except #%datum)

(require
  "../helpers.rkt"
  "ed-dynamics.rkt")

(provide #%datum #%app fun :ed)

;; Types
(define-type-constructor → #:arity = 2)

;; Statics, following PFPL 8.1a,b, with a redefinition of #%datum
;; for more convenient usage.
(define-typed-syntax #%datum
  [(_ . n:number) ≫
   --------
   [⊢ (#%datum- . n) ⇒ num]]
  [(_ . s:str) ≫
   --------
   [⊢ (#%datum- . s) ⇒ str]])

(define-typed-syntax (#%app f:id e) ≫
  [⊢ f ≫ f- ⇒ (~→ T_1 T_2)]
  [⊢ e ≫ e- ⇐ T_1]
  --------
  [⊢ (#%app- list 'apply (qo f-) (qo e-)) ⇒ T_2])

(define-typed-syntax fun #:datum-literals(: = in)
  [(_ f:id (x1:id : T_1:type) : T_2:type = e2 in e) ≫
   [[x1 ≫ x1- : T_1] ⊢ e2 ≫ e2- ⇐ T_2]
   [[f ≫ f- : (→ T_1 T_2)] ⊢ e ≫ e- ⇒ T]
   --------
   [⊢ (#%app- list 'fun (qo f-) (qo x1-) (qo e2-) (qo e-)) ⇒ T]])

(define-syntax-rule (:ed e)
 (#%app- evaluate e))