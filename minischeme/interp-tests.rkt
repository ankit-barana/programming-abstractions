#lang racket
; loki fondeur & ankit barana

(require rackunit)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define interp-tests
  (test-suite
   "Interpreter tests"
   
   (test-equal? "Number"
             (eval-exp (lit-exp 5) empty-env)
             5)
   (test-exn "Not a lit-exp"
                exn:fail?
                (λ () (eval-exp 'A empty-env)))
   (test-equal? "testing x"
               (eval-exp (var-exp 'x) init-env)
               23)
   (test-equal? "testing y"
               (eval-exp (var-exp 'y) init-env)
               42)
   (test-exn "Unbounded var-exp"
             exn:fail?
             (λ () (eval-exp (var-exp 'z) init-env)))

   ; app-exp tests
   (test-equal? "+"
                (eval-exp (app-exp (var-exp '+) (list (lit-exp 2) (lit-exp 3))) init-env)
                5)
   (test-equal? "-"
                (eval-exp (app-exp (var-exp '-) (list (lit-exp 2) (lit-exp 3))) init-env)
                -1)
   (test-equal? "*"
                (eval-exp (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) init-env)
                6)
   (test-equal? "/"
                (eval-exp (app-exp (var-exp '/) (list (lit-exp 2) (lit-exp 3))) init-env)
                (/ 2 3))
   (test-equal? "add1"
                (eval-exp (app-exp (var-exp 'add1) (list (lit-exp 1))) init-env)
                2)
   (test-equal? "sub1"
                (eval-exp (app-exp (var-exp 'sub1) (list (lit-exp 1))) init-env)
                0)
   (test-equal? "negate"
                (eval-exp (app-exp (var-exp 'negate) (list (lit-exp 1))) init-env)
                -1)
   (test-equal? "list"
                (eval-exp (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))) init-env)
                '(1 2 3))
   (test-equal? "cons"
                (eval-exp (app-exp (var-exp 'cons) (list (lit-exp 1) (app-exp (var-exp 'list) (list (lit-exp 2) (lit-exp 3))))) init-env)
                '(1 2 3))
   (test-equal? "car"
                (eval-exp (app-exp (var-exp 'car) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) init-env)
                1)
   (test-equal? "cdr"
                (eval-exp (app-exp (var-exp 'cdr) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) init-env)
                '(2 3))

   
   ; tests conditionals
   (test-equal? "If-then-else in which is cond is 0"
              (eval-exp (ite-exp (lit-exp 0) (lit-exp 1) (lit-exp 2)) empty-env)
                2)
   (test-equal? "If-then-else in which is cond is a number not 0"
              (eval-exp (ite-exp (lit-exp 3) (lit-exp 1) (lit-exp 2)) empty-env)
                1)
   (test-eqv? "If-then-else using 'False"
              (eval-exp (ite-exp (var-exp 'False) (lit-exp 5) (lit-exp 6)) init-env)
              6)
   (test-equal? "If-then-else - conditional, then, and else all are an applications"
              (eval-exp (ite-exp (app-exp (var-exp 'gt?) (list (var-exp 'y) (var-exp 'x))) (app-exp (var-exp 'add1) (list (var-exp 'y))) (app-exp (var-exp 'add1) (list (var-exp 'x)))) init-env) ; x = 23, y = 42
                43)

   ; tests let expression
   
   (test-equal? "single let single binding"

                ;(let ([a 0])
                ;  (sub1 a))
                
                (eval-exp (let-exp
                 '(a)
                 (list (lit-exp 0))
                 (app-exp (var-exp 'sub1) (list (var-exp 'a)))) init-env)
                -1)
   
   (test-equal? "single let many bindings"

                ;(let ([a 1]
                ;      [b 5])
                ;  (+ a b))
                
                (eval-exp (let-exp
                 '(a b)
                 (list (lit-exp 1) (lit-exp 5))
                 (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))) init-env)
                6)
   
   (test-equal? "nested let"

                ;(let ([a (* 2 3)]
                ;      [b 24])
                ;  (let ([c (- b a)])
                ;    (* c (+ a b))))
                
                (eval-exp (let-exp
                 '(a b)
                 (list (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) (lit-exp 24))
                 (let-exp
                  '(c)
                  (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                  (app-exp (var-exp '*) (list (var-exp 'c) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b))))))) init-env)
                540)
   
   (test-equal? "overwriting a variable"

                ;(let ([y (add1 x)]) y)
                
                (eval-exp (let-exp
                 '(y)
                 (list (app-exp (var-exp 'add1) (list (var-exp 'x))))
                 (var-exp 'y)) init-env)
                24)

   
   (test-equal? "single argument lambda"
                (eval-exp (app-exp (lambda-exp '(x) (var-exp 'x)) (list (lit-exp 1))) init-env)
                1)
   
   (test-equal? "multiple argument lambda"
                (eval-exp (app-exp (lambda-exp '(x y) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'y)))) (list (lit-exp 1) (lit-exp 2))) init-env)
                2)

   (test-equal? "let-lambda combination to apply lambda"
                (eval-exp (let-exp '(sqr) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'x)))))
                         (app-exp (var-exp 'sqr) (list (lit-exp 64)))) init-env)
                4096)

   (test-equal? "multiple lambdas inside let"
                (eval-exp (let-exp '(sqr) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'x)))))
                         (let-exp '(cube) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (app-exp (var-exp 'sqr) (list (var-exp 'x)))))))
                                  (app-exp (var-exp 'cube) (list (lit-exp 3))))) init-env)
                27)

   (test-equal? "Begin + set"
                (eval-exp (begin-exp (list (set-exp 'x (lit-exp 2))
                                           (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y)))))
                          init-env)
                44)

   (test-equal? "Set and begin in a let"
                (eval-exp (let-exp '(k y) (list (lit-exp 1) (lit-exp 2))
                           (begin-exp (list (set-exp 'k (lit-exp 5)) (app-exp (var-exp '+) (list (var-exp 'k) (var-exp 'y)))))) init-env)
                7)               
   ))
