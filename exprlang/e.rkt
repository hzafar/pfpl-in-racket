#lang turnstile/lang

(require
  "../helpers.rkt"
  "e-dynamics.rkt"
  (rename-in "e-dynamics-lazy.rkt" [evaluate lazy-evaluate]))

(provide num str #%datum + * ^ len let :e :e-lazy)

;; Types
(define-base-type num)
(define-base-type str)

;; Statics, following PFPL 41.a-h.
(define-typed-syntax #%datum
  [(_ . n:number) ≫
   --------
   [⊢ (#%datum- . n) ⇒ num]]
  [(_ . s:str) ≫
   --------
   [⊢ (#%datum- . s) ⇒ str]])

(define-typed-syntax (+ e1 e2) ≫
  [⊢ e1 ≫ e1- ⇐ num]
  [⊢ e2 ≫ e2- ⇐ num]
  --------
  [⊢ (list '+ (qo e1-) (qo e2-)) ⇒ num])

(define-typed-syntax (* e1 e2) ≫
  [⊢ e1 ≫ e1- ⇐ num]
  [⊢ e2 ≫ e2- ⇐ num]
  --------
  [⊢ (list '* (qo e1-) (qo e2-)) ⇒ num])

(define-typed-syntax (^ e1 e2) ≫
  [⊢ e1 ≫ e1- ⇐ str]
  [⊢ e2 ≫ e2- ⇐ str]
  --------
  [⊢ (list '^ (qo e1-) (qo e2-)) ⇒ str])

(define-typed-syntax (len e) ≫
  [⊢ e ≫ e- ⇐ str]
  --------
  [⊢ (list 'len (qo e-)) ⇒ num])

(define-typed-syntax (let x:id (~literal be) e1 (~literal in) e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ_1]
  [[x ≫ x- : τ_1] ⊢ e2 ≫ e2- ⇒ τ_2]
  --------
  [⊢ (list 'let (qo x-) (qo e1-) (qo e2-)) ⇒ τ_2])

;; Wrappers for E programs.
(define-syntax-rule (:e e)
 (evaluate e))

(define-syntax-rule (:e-lazy e)
  (lazy-evaluate e))