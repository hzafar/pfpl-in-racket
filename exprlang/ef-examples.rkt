#lang s-exp "ef.rkt"

(:ef ((λ (x : str) (+ (len x) 4)) "hello"))

(:ef (let k be (λ (x1 : num) (λ (x2 : num) x1))
      in (let kz be (k 0) in (+ (kz 3) (kz 5)))))

