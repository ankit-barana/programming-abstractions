#lang racket
; loki fondeur & ankit barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")
(provide env-tests)

; rpocedures available:
;   env
;   env?
;   empty-env
;   empty-env?
;   env-syms
;   env-vals
;   env-previous
;   env-lookup


; Define an environment for testing.
(define env-prev
  (env '(x y) '(1 2) empty-env))

(define env-current
  (env '(x a b) '(3 4 5) env-prev))

(define env-tests
  (test-suite
   "Environment tests"
   
   (test-equal? "tests empty-env?"
                    (empty-env? empty-env)
                #t)
   
   (test-equal? "tests env? on prev env"
                    (env? env-prev)
                #t)
   
   (test-equal? "tests env? on current env"
                    (env? env-current)
                #t)
   
   (test-equal? "tests env-syms on prev env"
                    (env-syms env-prev)
                '(x y))
   
   (test-equal? "tests env-syms on current env"
                    (env-syms env-current)
                '(x a b))

   (test-false "tests failure of env-syms on current env"
                    (equal? (env-syms env-current)
                '(x y)))
   
   (test-equal? "tests env-vals on prev env"
                    (env-vals env-prev)
                '(1 2))
   
   (test-equal? "tests env-vals on current env"
                    (env-vals env-current)
                '(3 4 5))

   (test-false "tests failure of env-vals on current env"
                    (equal? (env-vals env-current)
                '(1 2)))
   
   (test-equal? "tests env-prev on a non-empty env"
                    (env-previous env-current)
                env-prev)

   (test-exn "Empty environment has no previous"
                    exn:fail?
                (λ () (env-previous empty-env)))
   
   (test-equal? "tests env-lookup in outermost env the when value is present"
                    (env-lookup env-prev 'x)
                1)

   (test-exn "symbol is not present in any env"
                    exn:fail?
                (λ () ((env-lookup env-current 'z))))
   
   (test-equal? "tests env-lookup in inner env when symbol is in both inner and outer env"
                    (env-lookup env-current 'x) ; should return the inner value 
                3)
   
   (test-equal? "tests env-lookup in outer env when symbol is in both inner and outer env"
                    (env-lookup env-prev 'x) ; should return the outer value value
                1)
   
   (test-exn "tests env-lookup from outer env when symbol is only in inner env"
                    exn:fail?
                (λ () ((env-lookup env-prev 'a)))) ; should fail
   
   (test-equal? "tests env-lookup in innermost env when symbol is at the outer env"
                    (env-lookup env-current 'y)
                2)))


