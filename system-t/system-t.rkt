#lang turnstile/lang

(require
  "system-t-dynamics.rkt"
  (rename-in "system-t-dynamics-lazy.rkt" [evaluate lazy:evaluate]))

(provide Nat → s z λ rec #%app :t :t-lazy)

;; A helper macro that essentially prevents "double-quoting" expressions,
;; whereas plain quote would give us things like '(succ '(succ '0)), due
;; to how the typechecking macros are expanded.
(define-syntax (qt-once stx)
  (syntax-case stx ()
    [(_ expr)
     (if (symbol? (syntax->datum #'expr))
         #'`expr
         #'expr)]))

;; Types
(define-base-type Nat)
(define-type-constructor → #:arity = 2)

;; Statics, following PFPL 9.1a-f.
;; The statics typecheck a System T expression and generate a quoted list
;; representation that can then be passed to the evaluation function defined
;; in system-t-dynamics.rkt. This allows us to control our own evaluation
;; dynamics, independent of Racket's dynamics.
(define-typed-syntax z
  [(~literal z) ≫
   --------
   [⊢ 0 ⇒ Nat]])

(define-typed-syntax (s e) ≫
  [⊢ e ≫ e- ⇐ Nat]
  --------
  [⊢ (#%app- list 'succ (qt-once e-)) ⇒ Nat])

(define-typed-syntax (λ (x:id : T_1:type) e) ≫
  [[x ≫ x- : T_1] ⊢ e ≫ e- ⇒ T_2]
  --------
  ; some massaging to handle argument scoping correctly
  [⊢ (let- ([x-- (#%app- gensym)])
           (#%app- list 'lam x-- (#%app- substitute x-- (qt-once x-) (qt-once e-))))
     ⇒ (→ T_1 T_2)])

(define-typed-syntax rec #:datum-literals (\{ \} ↪ s z with)
  [(_ e {z ↪ e0 s (x:id) with y:id ↪ e1}) ≫
   [⊢ e ≫ e- ⇐ Nat]
   [⊢ e0 ≫ e0- ⇒ T]
   [[x ≫ x- : Nat] [y ≫ y- : T] ⊢ e1 ≫ e1- ⇐ T]
   --------
   [⊢ (#%app- list 'rec (qt-once e-) (qt-once e0-) (qt-once x-) (qt-once y-) (qt-once e1-)) ⇒ T]])

(define-typed-syntax (#%app e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ (~→ T_in T_out)]
  [⊢ e2 ≫ e2- ⇐ T_in]
  --------
  [⊢ (#%app- list 'ap (qt-once e1-) (qt-once e2-)) ⇒ T_out])

;; Wrappers for System T expressions which pass the (type-checked) quoted
;; list representation to the evaluator. This connects the statics, given
;; via Turnstile above, with a particular evaluation dynamics.
(define-syntax-rule (:t e)
 (#%app- to-number (#%app- evaluate e)))

(define-syntax-rule (:t-lazy e)
  (#%app- lazy:evaluate e))