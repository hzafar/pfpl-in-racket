#lang turnstile/lang

(require
  "../helpers.rkt"
  "system-t-dynamics.rkt"
  (rename-in "system-t-dynamics-lazy.rkt" [evaluate lazy-evaluate]))

(provide nat → s z λ rec #%app :t :t-lazy)

;; Types
(define-base-type nat)
(define-type-constructor → #:arity = 2)

;; Statics, following PFPL 9.1a-f.
;; The statics typecheck a System T expression and generate a quoted list
;; representation that can then be passed to the evaluation function defined
;; in system-t-dynamics.rkt. This allows us to control our own evaluation
;; dynamics, independent of Racket's dynamics.
(define-typed-syntax z
  [(~literal z) ≫
   --------
   [⊢ 0 ⇒ nat]])

(define-typed-syntax (s e) ≫
  [⊢ e ≫ e- ⇐ nat]
  --------
  [⊢ (#%app- list 'succ (qo e-)) ⇒ nat])

(define-typed-syntax (λ (x:id : τ_1:type) e) ≫
  [[x ≫ x- : τ_1] ⊢ e ≫ e- ⇒ τ_2]
  --------
  ; some massaging to handle argument scoping correctly
  [⊢ (let- ([x-- (#%app- gensym)])
           (#%app- list 'lam x-- (#%app- substitute x-- (qo x-) (qo e-))))
     ⇒ (→ τ_1 τ_2)])

(define-typed-syntax rec #:datum-literals (\{ \} ↪ s z with)
  [(_ e {z ↪ e0 s (x:id) with y:id ↪ e1}) ≫
   [⊢ e ≫ e- ⇐ nat]
   [⊢ e0 ≫ e0- ⇒ τ]
   [[x ≫ x- : nat] [y ≫ y- : τ] ⊢ e1 ≫ e1- ⇐ τ]
   --------
   [⊢ (#%app- list 'rec (qo e-) (qo e0-) (qo x-) (qo y-) (qo e1-)) ⇒ τ]])

(define-typed-syntax (#%app e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ (~→ τ_in τ_out)]
  [⊢ e2 ≫ e2- ⇐ τ_in]
  --------
  [⊢ (#%app- list 'ap (qo e1-) (qo e2-)) ⇒ τ_out])

;; Wrappers for System T expressions which pass the (type-checked) quoted
;; list representation to the evaluator. This connects the statics, given
;; via Turnstile above, with a particular evaluation dynamics.
(define-syntax-rule (:t e)
 (#%app- to-number (#%app- evaluate e)))

(define-syntax-rule (:t-lazy e)
  (#%app- lazy-evaluate e))