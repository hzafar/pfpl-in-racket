#lang s-exp "system-t.rkt"

;; Some example System T programs.

;; 0
(:t z)
(:t-lazy z)

;; successor
(:t (s (s z)))
(:t-lazy (s (s z)))

;; λ application
(:t ((λ (x : nat) (s x)) z))
(:t-lazy ((λ (x : nat) (s x)) z))

;; (+ 1 2)
(:t (rec (s z) {z ↪ (s (s z))
                s(x) with y ↪ (s y)}))
(:t-lazy (rec (s z) {z ↪ (s (s z))
                     s(x) with y ↪ (s y)}))

;; once more, with λs
(:t (((λ (a : nat) (λ (b : nat) (rec a {z ↪ b
                                        s(x) with y ↪ (s y)}))) (s z)) (s (s z))))
(:t-lazy (((λ (a : nat) (λ (b : nat) (rec a {z ↪ b
                                             s(x) with y ↪ (s y)}))) (s z)) (s (s z))))

;; (- 3 1)
(:t (rec (s z) {z ↪ (s (s (s z)))
                s(x) with y ↪ (rec y {z ↪ z
                                      s(a) with b ↪ a})}))
(:t-lazy (rec (s z) {z ↪ (s (s (s z)))
                     s(x) with y ↪ (rec y {z ↪ z
                                           s(a) with b ↪ a})}))

;; type error
;(:t ((λ (x : nat) x) (λ (x : nat) x))) ; => type mismatch: expected nat, given (→ nat nat)