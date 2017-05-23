#lang s-exp "e.rkt"

(:e 42)
(:e-lazy 42)

(:e (+ (+ 1 (* 2 3)) (+ 8 9)))
(:e-lazy (+ (+ 1 (* 2 3)) (+ 8 9)))

(:e (^ "good" "night"))
(:e-lazy (^ "good" "night"))

(:e (len "Westernesse"))
(:e-lazy (len "Westernesse"))

(:e (let x be 5 in (+ x 4)))
(:e-lazy (let x be 5 in (+ x 4)))

(:e (let y be ", world!" in (^ "Hello" y)))
(:e-lazy (let y be ", world!" in (^ "Hello" y)))
