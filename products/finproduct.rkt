#lang racket

; Can't figure out how to express the finite product typing
; in Turnstile. The stlc+reco+var example in the repo is a bit
; mysterious. Using plain Racket until I up my macro game...

;; Types
(struct × (labels types) #:transparent)
(define Num 'Num) ;; add a base type for testing

;; Statics, vaguely having something to do with PFPL 10.3a,b.
(define (expr-typchk expr)
  (cond [(number? expr) Num]
        [(equal? (car expr) 'tpl)
         (tuple-typchk expr)]
        [(equal? (car expr) 'prj)
         (let ([tpl-type (tuple-typchk (cadr expr))])
           (if (×? tpl-type)
               (let ([i (index-of (×-labels tpl-type) (caddr expr))])
                 (if (not i)
                     (error "Cannot project unknown label:" (caddr expr))
                     (list-ref (×-types tpl-type) i)))
               (error "Cannot project on non-tuple expression:" expr)))]
        [else (error "Unknown expression type:" expr)]))

(define (tuple-typchk expr)
  (cond [(null? (cdr expr)) (× '() '())]
        [else
         (let ([expr-types (map expr-typchk (map cadr (cdr expr)))])
           (× (map car (cdr expr)) expr-types))]))

;; e.g
(tuple-typchk '(tpl))
(tuple-typchk '(tpl (day 22) (month 3)))
(tuple-typchk '(tpl (x 1) (y (tpl (a 0) (b 10) (c 22)))))
(expr-typchk '(prj (tpl (x 4) (y (tpl (z 3)))) y))
;(expr-typchk '(prj (tpl (a 0) (b 0)) c))