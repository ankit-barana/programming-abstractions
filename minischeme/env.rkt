#lang racket
; loki fondeur & ankit barana

; exports all the given procedures 
(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup)

; The empty environment is null.
(define empty-env null)
(define empty-env? null?)

; Define the env data type here
(struct env (syms vals previous) #:transparent)

(define (env-lookup environment symbol)
  ; if environement is empty, throw an error
  (cond [(empty-env? environment)(error 'env-lookup "No binding for ~s" symbol)]
        ; if symbol is present in current environment, return its value
        [(member symbol (env-syms environment)) (foldr (lambda(syms vals init) (if (equal? symbol syms) vals init)) 0 (env-syms environment) (env-vals environment))]
        ; otherwise, we lookup the previous environment
        [else (env-lookup (env-previous environment) symbol)]
        ))