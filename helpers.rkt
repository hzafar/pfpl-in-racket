#lang racket

(provide qo substitute)

;; A helper macro that essentially prevents "double-quoting" expressions,
;; whereas plain quote would give us things like '(succ '(succ '0)), due
;; to how the typechecking macros are expanded.
(define-syntax (qo stx)
  (syntax-case stx ()
    [(_ expr)
     (if (symbol? (syntax->datum #'expr))
         #'`expr
         #'expr)]))

(define (substitute val var expr)
  (cond [(empty? expr) empty]
        [(equal? var expr) val]
        [(or (symbol? expr) (string? expr) (number? expr)) expr]
        [(equal? var (first expr)) (cons val (substitute val var (rest expr)))] 
        [(cons? (first expr)) (cons (substitute val var (first expr)) (substitute val var (rest expr)))]
        [else (cons (first expr) (substitute val var (rest expr)))]))