#lang racket
; loki fondeur & ankit barana

(provide lit-exp lit-exp? lit-exp-num ;from lit-exp 
         var-exp var-exp? var-exp-sym ; from var-exp 
         app-exp app-exp? app-exp-proc app-exp-args ;from app-exp 
         ite-exp ite-exp? ite-exp-cond ite-exp-then ite-exp-else ; from ite-exp 
         let-exp let-exp? let-exp-sym let-exp-exp let-exp-body ; from let-exp
         lambda-exp lambda-exp? lambda-exp-args lambda-exp-body ; from lambda-exp
         set-exp set-exp? set-exp-sym set-exp-exp ; from sym-exp
         begin-exp begin-exp? begin-exp-exps ; from begin-exp
         parse
         )

(struct lit-exp (num) #:transparent)
(struct var-exp (sym) #:transparent)
(struct app-exp (proc args) #:transparent)
(struct ite-exp (cond then else) #:transparent)
(struct let-exp (sym exp body) #:transparent)
(struct lambda-exp (args body) #:transparent)
(struct set-exp (sym exp) #:transparent)
(struct begin-exp (exps) #:transparent)

(define (valid? lst)
  (cond [(empty? lst) #t]
        [(eq? (length (first lst)) 2) (valid? (rest lst))]
        [else (parse-helper lst)]))

(define (parse-helper input) (error 'parse "Invalid syntax ~s" input))


(define (parse input) ;input = user input from minischeme command line
  (cond [(number? input) (lit-exp input)] ; if input is a number, we create a lit-exp 
        [(symbol? input) (var-exp input)] ; if input is a symbol, we create a var-exp
        [(list? input) ;if the input is a list...
              (cond [(empty? input) (parse-helper input)] ; if the list is empty, we throw an error
                    [(eq? (first input) 'if) (if (eq? (length input) 4)
                                                 (ite-exp (parse (second input)) (parse (third input)) (parse (fourth input)))
                                                 (parse-helper input))] ; if the list has 4 elements and its first value  is 'if symbol , we create an ite expression
                    
                 [(eq? (first input) 'let) (if (eq? (length input) 3)
                                               (if (valid? (second input))
                                                   (let-exp (map first (second input)) (map parse (map second (second input))) (parse (third input)))
                                                   (parse-helper))
                                               (parse-helper))]

                 [(and (eq? (first input) 'lambda) (eq? (length input) 3)) (lambda-exp (second input) (parse (third input)))]
                 [(eq? (first input) 'set!) (if (= (length input) 3)
                                                (set-exp (second input) (parse (third input)))
                                                (parse-helper))]
                 [(eq? (first input) 'begin) (begin-exp (map parse (rest input)))]
                 [(equal? (first input) 'letrec) (if (equal? (length input) 3)
                                                     (parse-letrec input)
                                                     (parse-helper input))]

                 [else (app-exp (parse (first input)) (map parse(rest input)))])] ;otherwise, we create an app-exp from the given proc and args
        [else (parse-helper input)])) ;throws error otherwise

(define (parse-letrec input)
  (let ([syms (map first (second input))]
        [exps (map second (second input))]
        [body (third input)])
    (let ((values (map (lambda (s) (lit-exp 0)) syms)))
      (let ((new-syms (map (lambda (s) (gensym)) syms)))
        (let-exp syms values
                (let ((parsed-exps (map (lambda (exp) (parse exp)) exps)))
                  (let-exp new-syms parsed-exps
                          (begin-exp
                            (foldr (lambda (s new-s acc)
                                     (cons (set-exp s (parse new-s)) acc))
                                   (list (parse body))
                                   syms
                                   new-syms)))))))))