#lang s-exp "ed.rkt"

(:ed (fun f(x : str) : num = (+ (len x) 4) in (* (f "hello") (f "world"))))