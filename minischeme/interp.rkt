#lang racket
; loki fondeur & ankit barana

; for part g, we mapped box onto the values in env

(require "parse.rkt" "env.rkt")
(provide eval-exp init-env)

(struct prim-proc (op) #:transparent)

(struct closure (params body env) #:transparent)

(define primitive-operators '(+ - * / add1 sub1 negate list cons car cdr  ;primitive procedures
                                eqv? lt? gt? leq? geq? null? list? number?)) ; bool procedures

(define prim-env
  (env primitive-operators
       (map box (map prim-proc primitive-operators))
       empty-env))

(define init-env
  (env '(x y null True False) (map box '(23 42 () True False)) prim-env))

(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (unbox (env-lookup e (var-exp-sym tree)))]
        
        ;apply-proc takes the result of the curried map of eval-expthe and var-exp (prim proc) after it's been evaluated
        ;in order to turn the arguments from "lit-exp" or "var-exp" to their values
        [(app-exp? tree) (apply-proc
                                    (eval-exp (app-exp-proc tree) e)
                                    (map (lambda (args)
                                           (eval-exp args e))
                                         (app-exp-args tree)))]
        
        ; if condition is True or 0, then exp is evalualted. Otherwise, else exp is evaluated
        [(ite-exp? tree) (if (or (eq? (eval-exp (ite-exp-cond tree) e) 0) (eq? (eval-exp (ite-exp-cond tree) e) 'False))
                             (eval-exp (ite-exp-else tree) e)
                             (eval-exp (ite-exp-then tree) e))]

        [(let-exp? tree) (eval-exp (let-exp-body tree) (env (let-exp-sym tree) (map box (map (lambda (exp) (eval-exp exp e)) (let-exp-exp tree))) e))]
        
        [(lambda-exp? tree) (closure (lambda-exp-args tree) (lambda-exp-body tree) e)] ;if tree is a lambda-exp, create a closure

        [(set-exp? tree) (set-box! (env-lookup e (set-exp-sym tree)) (eval-exp (set-exp-exp tree) e))]
        
        [(begin-exp? tree) (foldl (lambda (exp acc) (eval-exp exp e))
                                  (void)
                                  (begin-exp-exps tree))]
                                                                                                  
        [else (error 'eval-exp "Invalid syntax ~s" tree)]))
  
(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc)
         (eval-exp (closure-body proc) (env (closure-params proc) (map box args) (closure-env proc)))]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1) (apply add1 args)]
        [(eq? op 'sub1) (apply sub1 args)]
        [(eq? op 'negate) (apply (lambda (num) (* -1 num)) args)]
        [(eq? op 'list) (apply list args)]
        [(eq? op 'cons) (apply cons args)]
        [(eq? op 'car) (apply car args)]
        [(eq? op 'cdr) (apply cdr args)]
        [(eq? op 'eqv?) (if (apply eqv? args) 'True 'False)]
        [(eq? op 'lt?) (if (apply < args) 'True 'False)]
        [(eq? op 'gt?) (if (apply > args) 'True 'False)]
        [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
        [(eq? op 'geq?) (if (apply >= args) 'True 'False)]
        [(eq? op 'null?) (if (apply null? args) 'True 'False)]
        [(eq? op 'list?) (if (apply list? args) 'True 'False)]
        [(eq? op 'number?) (if (apply number? args) 'True 'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))
