#lang racket
; loki fondeur & ankit barana

(require rackunit)
(require "parse.rkt")
(provide parse-tests)


; available procedures
;     lit-exp
;     lit-exp?
;     lit-exp-num
;     var-exp
;     var-exp?
;     var-exp-sym
;     lambda-exp
;     lambda-exp?
;     parse


; parsing example
(define parsed_num (parse 2))
(define parsed_sym (parse 'x))
(define parsed_proc (parse (list '+ 1 2)))

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Symbol"
           var-exp?
           (parse '2A))
   
   (test-pred "Literal"
           lit-exp?
           (parse 5))
   
   (test-equal? "Accessing number from a lit-exp"
           (lit-exp-num parsed_num)
           2)
   
   (test-equal? "Accessing symbol from a var-exp"
           (var-exp-sym parsed_sym)
           'x)
   (test-equal? "Accessing proc from app-exp"
                (app-exp-proc parsed_proc)
                (var-exp '+))
   (test-equal? "parsing apps list len 1"
                (parse (list 'foo))
                (app-exp (var-exp 'foo) '()))
   (test-equal? "parsing apps list len 2"
                (parse (list 'foo 2))
                (app-exp (var-exp 'foo) (list (lit-exp 2))))
   (test-equal? "parsing apps list len 3"
                (parse (list 'foo 2 3))
                (app-exp (var-exp 'foo) (list (lit-exp 2) (lit-exp 3))))
   (test-exn "empty list"
                    exn:fail?
                (λ () (parse (list))))

   ; app-exp tests
   (test-pred "app-exp?"
              app-exp?
                (parse '(+ 1)))
   (test-pred "app-exp? - multiple arguments"
              app-exp?
                (parse '(+ 1 2 3 4)))
   (test-pred "app-exp? - multiple procs"
              app-exp?
                (parse '(+ 1 (- 7 4))))
   (test-equal? "parsed output single proc"
                (parse '(+ 2 3))
                (app-exp (var-exp '+) (list (lit-exp 2) (lit-exp 3))))
   (test-equal? "parsed output multiple procs"
                (parse '(+ 1 (- 7 4)))
                (app-exp (var-exp '+) (list (lit-exp 1) (app-exp (var-exp '-) (list (lit-exp 7) (lit-exp 4))))))  
   (test-equal? "parsed output multiple procs"
                (parse '(+ 1 (- 7 4)))
                (app-exp (var-exp '+) (list (lit-exp 1) (app-exp (var-exp '-) (list (lit-exp 7) (lit-exp 4))))))
   (test-equal? "parsed output multiple procs"
                (parse '(+ 1 (- 7 4)))
                (app-exp (var-exp '+) (list (lit-exp 1) (app-exp (var-exp '-) (list (lit-exp 7) (lit-exp 4))))))
   (test-equal? "app-exp-proc"
                (app-exp-proc (parse '(+ 2 3)))
                (var-exp '+))
   (test-equal? "app-exp-proc"
                (app-exp-args (parse '(+ 2 3)))
                (list (lit-exp 2) (lit-exp 3)))



   ; if-then-else tests
   (test-pred "ite-exp?"
                ite-exp?
                (parse '(if foo 1 2)))
   (test-pred "ite-exp? if then else are application"
                ite-exp?
                (parse '(if (<= 2 3)  (+ 2 3) (- 2 3))))
   
   (test-equal? "ite-exp-cond"
                (ite-exp-cond (parse '(if (> 2 4) 3 f)))
                (app-exp (var-exp '>) (list (lit-exp 2) (lit-exp 4))))
   (test-equal? "ite-exp-then"
                (ite-exp-then (parse '(if (> 2 4) 3 f)))
                (lit-exp 3))
   (test-equal? "ite-exp-else"
                (ite-exp-else (parse '(if (> 2 4) 3 f)))
                (var-exp 'f))

   (test-equal? "If-then-else condition is application"
                (parse '(if (foo) 3 y))
                (ite-exp (app-exp (var-exp 'foo) '()) (lit-exp 3) (var-exp 'y)))

   (test-equal? "ite - then is application"
                (parse '(if x (> 2 4) y))
                (ite-exp (var-exp 'x) (app-exp (var-exp '>) (list (lit-exp 2) (lit-exp 4))) (var-exp 'y)))

   (test-equal? "ite - else is application"
                (parse '(if cond 0 (foo)))
                (ite-exp (var-exp 'cond) (lit-exp 0) (app-exp (var-exp 'foo) '())))

   ; let-exp tests
   (test-pred "let-exp?"
                let-exp?
                (parse '(let ([a 1]
                              [b 5])
                          (+ a b))))
   (test-equal? "let-exp-sym"
                (let-exp-sym (parse '(let ([a 1]
                                           [b 5])
                                       (+ a b))))
                '(a b))
   (test-equal? "let-exp-exp"
                (let-exp-exp (parse '(let ([a 1]
                                           [b 5])
                                       (+ a b))))
                (list (lit-exp 1) (lit-exp 5)))
   (test-equal? "let-exp-body"
                (let-exp-body (parse '(let ([a 1]
                                           [b 5])
                                       (+ a b))))
                (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b))))
   (test-equal? "let-exp single let, multiple bindings"
                (parse '(let ([a 0])
                          (sub1 a)))
                (let-exp
                 '(a)
                 (list (lit-exp 0))
                 (app-exp (var-exp 'sub1) (list (var-exp 'a)))))
   (test-equal? "let-exp single let, multiple bindings"
                (parse '(let ([a 1]
                              [b 5])
                          (+ a b)))
                (let-exp
                 '(a b)
                 (list (lit-exp 1) (lit-exp 5))
                 (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))
   
   (test-equal? "nested let"
                (parse '(let ([a (* 2 3)]
                              [b 24])
                          (let ([c (- b a)])
                            (* c (+ a b)))))
                (let-exp
                 '(a b)
                 (list (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) (lit-exp 24))
                 (let-exp
                  '(c)
                  (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                  (app-exp (var-exp '*) (list (var-exp 'c) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b))))))))
   
   (test-equal? "overwriting a variable"
                (parse '(let ([y (add1 x)]) y))
                (let-exp
                 '(y)
                 (list (app-exp (var-exp 'add1) (list (var-exp 'x)) ))
                 (var-exp 'y)))


   ; lambdas  
   (test-pred "lambda-exp?"
              lambda-exp?
              (parse '(lambda (x) (+ x 1))))
   
   (test-equal? "lambda-exp-args"
                (lambda-exp-args (parse '(lambda (x y) (* x y))))
                '(x y))

   (test-equal? "lambda-exp-body"
                (lambda-exp-body (parse '(lambda (x) (* x x))))
                (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'x))))

   (test-equal? "single argument lambda"
                (parse '((lambda (x) x) 1))
                (app-exp (lambda-exp '(x) (var-exp 'x)) (list (lit-exp 1))))
   
   (test-equal? "multiple argument lambda"
                (parse '((lambda (x y) (* x y)) 1 2))
                (app-exp (lambda-exp '(x y) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'y)))) (list (lit-exp 1) (lit-exp 2))))

   (test-equal? "let-lambda combination to apply lambda"
                (parse '(let ([sqr (lambda (x) (* x x))]) (sqr 64)))
                (let-exp '(sqr) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'x)))))
                         (app-exp (var-exp 'sqr) (list (lit-exp 64)))))

   (test-equal? "let with multiple lambdas"
                (parse '(let ([sqr (lambda (x) (* x x))]) (let ([cube (lambda (x) (* x (sqr x)))]) (cube 3))))
                (let-exp '(sqr) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (var-exp 'x)))))
                         (let-exp '(cube) (list (lambda-exp '(x) (app-exp (var-exp '* ) (list (var-exp 'x) (app-exp (var-exp 'sqr) (list (var-exp 'x)))))))
                                  (app-exp (var-exp 'cube) (list (lit-exp 3))))))


   ; set! tests
   (test-pred "set-exp?"
              set-exp?
              (parse '(set! x 42)))

   (test-equal? "set-exp-sym"
                (set-exp-sym (parse '(set! x 42)))
                'x)

   (test-equal? "set-exp-exp"
                (set-exp-exp (parse '(set! x 42)))
                (lit-exp 42))
   
   (test-equal? "set with a variable and value "
                (parse '(set! x 42))
                (set-exp 'x (lit-exp 42)))
   
   (test-equal? "set with a variable and an expression "
                (parse '(set! y (* x 2)))
                (set-exp 'y (app-exp (var-exp '*) (list (var-exp 'x) (lit-exp 2)))))


   ; begin tests
   
   (test-pred "begin-exp? - single expression"
              begin-exp?
              (parse '(begin (+ 1 2))))
   
   (test-pred "begin-exp? - multiple expression" 
              begin-exp?
              (parse '(begin (set! x 23)
                             (+ x y))))

   (test-equal? "begin-exp-exps returns all the expressions"
                (begin-exp-exps (parse '(begin (+ 1 2) (* 3 4))))
                (list (app-exp (var-exp '+) (list (lit-exp 1) (lit-exp 2)))
                      (app-exp (var-exp '*) (list (lit-exp 3) (lit-exp 4)))))
   
   (test-equal? "begin with single expression"
                (parse '(begin (+ 1 2)))
                (begin-exp (list (app-exp (var-exp '+) (list (lit-exp 1) (lit-exp 2))))))

   (test-equal? "begin with multiple expressions"
                (parse '(begin (+ 1 2) (* 3 4)))
                (begin-exp (list (app-exp (var-exp '+) (list (lit-exp 1) (lit-exp 2)))
                                 (app-exp (var-exp '* ) (list (lit-exp 3) (lit-exp 4))))))

   (test-equal? "nested begin expressions"
                (parse '(begin (set! x 10)
                               (begin (set! y 20)
                                      (set! z 30))))
                (begin-exp (list (set-exp 'x (lit-exp 10))
                                 (begin-exp (list (set-exp 'y (lit-exp 20))
                                                  (set-exp 'z (lit-exp 30)))))))

   (test-equal? "set-begin in ite-exp in let-exp"
                (parse '(let ([x 1] [y 2])
                          (if (gt? x y)
                              (begin (set! x 24) x)
                              (begin (set! y 25) y))))
                (let-exp
                 '(x y)
                 (list (lit-exp 1) (lit-exp 2))
                 (ite-exp
                  (app-exp (var-exp 'gt?) (list (var-exp 'x) (var-exp 'y)))
                  (begin-exp (list (set-exp 'x (lit-exp 24)) (var-exp 'x)))
                  (begin-exp (list (set-exp 'y (lit-exp 25)) (var-exp 'y))))))
   

   ; letrec tests
   (test-pred "Letrec"
              let-exp?
              (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))

   (test-pred "Letrec"
              let-exp?
              (let-exp-body (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 10)))))

   (test-pred "Letrec is a sequence of expressions"
              begin-exp?
              (let-exp-body (let-exp-body (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))))))