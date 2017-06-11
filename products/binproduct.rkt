#lang turnstile/lang

(require
  "../helpers.rkt"
  "binproduct-dynamics.rkt")

(provide unit × ⟨⟩ pair l r :bp)

;; Types
(define-base-type unit)
(define-type-constructor × #:arity = 2)

;; Statics, following PFPL 10.1a-d.
(define-typed-syntax ⟨⟩
  [(~literal ⟨⟩) ≫
  --------
  [⊢ 'null ⇒ unit]])

(define-typed-syntax (pair e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ_1]
  [⊢ e2 ≫ e2- ⇒ τ_2]
  --------
  [⊢ (list 'pair (qo e1-) (qo e2-)) ⇒ (× τ_1 τ_2)])

(define-typed-syntax (l e) ≫
  [⊢ e ≫ e- ⇒ (~× τ_1 τ_2)]
  --------
 [⊢ (list 'left (qo e-)) ⇒ τ_1])

(define-typed-syntax (r e) ≫
  [⊢ e ≫ e- ⇒ (~× τ_1 τ_2)]
  --------
  [⊢ (list 'right (qo e-)) ⇒ τ_2])

;; Wrapper
(define-syntax-rule (:bp e)
  (evaluate e))