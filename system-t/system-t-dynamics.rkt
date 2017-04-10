#lang racket

(provide evaluate to-number substitute)

(define (expr-type=? e t) (and (list? e) (equal? (car e) t)))

;; Definitions for recognizing and extracting information from
;; the different expression types.
(define (z? e) (and (number? e) (zero? e)))

(define (succ? e) (expr-type=? e 'succ))
(define (succ-expr e) (cadr e))

(define (lam? e) (expr-type=? e 'lam))
(define (lam-arg e) (cadr e))
(define (lam-body e) (caddr e))

(define (ap? e) (expr-type=? e 'ap))
(define (ap-fn e) (cadr e))
(define (ap-arg e) (caddr e))

(define (rec? e) (expr-type=? e 'rec))
(define rec-expr cadr)
(define rec-e0 caddr)
(define rec-e1-x cadddr)
(define (rec-e1-y e) (car (cddddr e)))
(define (rec-e1-expr e) (cadr (cddddr e)))

;; A closed value is either 0, a successor expression where the
;; argument is a closed value, or a lambda expression.
(define (val? e)
  (or (z? e)
      (and (succ? e) (val? (succ-expr e)))
      (lam? e)))

;; The evaluator steps over the expression and proceeds according to the type,
;; following the dynamics given in PFPL 9.2a-g.
(define (evaluate e)
  (cond [(val? e) e]
        [(succ? e) (evaluate `(succ ,(evaluate (succ-expr e))))]
        [(ap? e)
         (let ([fn (ap-fn e)]
               [arg (ap-arg e)])
           (cond [(and (lam? fn) (val? arg))
                  (evaluate (substitute arg (lam-arg fn) (lam-body fn)))]
                 [(val? fn) (evaluate `(ap ,fn ,(evaluate arg)))]
                 [else (evaluate `(ap ,(evaluate fn) ,arg))]))]
        [(rec? e)
         (let ([expr (rec-expr e)]
               [e0 (rec-e0 e)]
               [x (rec-e1-x e)]
               [y (rec-e1-y e)]
               [e1 (rec-e1-expr e)])
           (cond [(z? expr) (evaluate e0)]
                 [(and (succ? expr) (val? expr))
                  (evaluate (substitute `(rec ,(succ-expr expr) ,e0 ,x ,y ,e1) y (substitute (succ-expr expr) x e1)))]
                 [else (evaluate `(rec ,(evaluate expr) ,e0 ,x ,y ,e1))]))]
        [else (error "Unrecognized expression!" e)]))

;; Helper functions
(define (to-number e)
  (cond [(z? e) 0]
        [(succ? e) (add1 (to-number (succ-expr e)))]))

(define (substitute val var expr)
  (cond [(empty? expr) empty]
        [(equal? var expr) val]
        [(symbol? expr) expr]
        [(equal? var (first expr)) (cons val (substitute val var (rest expr)))] 
        [(cons? (first expr)) (cons (substitute val var (first expr)) (substitute val var (rest expr)))]
        [else (cons (first expr) (substitute val var (rest expr)))]))
