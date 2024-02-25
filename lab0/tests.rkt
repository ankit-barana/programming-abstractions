#lang racket
; Ankit Barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw0.rkt")


; Define tests for length
#| (define length-tests
  (test-suite
    "length"
     (test-equal? "Example list"
                (len (list 1 2 3))
                3))) |#

; Define tests for length
(define length-tests
  (test-suite
    "length"
     
     (test-equal? "Typical list - consisting elements of same type"
                  (len (list 1 2 3))
                3)  
     (test-equal? "Empty List"
                  (len '())
                0)
     (test-equal? "List with only one element"
                  (len '(1))
                1)
     (test-equal? "List with duplicates element"
                  (len '(1 1 1 1 1 1))
                6)
     (test-equal? "A large List"
                  (len (range 1000))
                1000)
     (test-equal? "List with multiple data type"
                 (len '(#t 1 5 "String" #\a))
                5)
     (test-equal? "Nested lists only"
                 (len '('(1 2) '(2 4 '(6 7) 5)))
                2)
     (test-equal? "Empty Nested List"
                  (len '('() '() '()))
                3)
     (test-equal? "List with nested lists and individual elements"
                  (len '('() '() 5 7))
                4)
     (test-equal? "Symbols"
                  (len '('CSCI, 'MATH, 'STAT, 'NSCI))
                4)))

; tests for arith
(define arith-tests
  (test-suite
    "Arith"
     
     (test-equal? "n >= 10 and even --> n + 2"
                  (arith 10)
                12)
     (test-equal? "n >= 10 and odd --> n"
                  (arith 11)
                11)
     (test-equal? "n < 10 and even --> n * 5"
                  (arith 8)
                40)
     (test-equal? "n < 10 and odd --> n + n"
                  (arith 9)
                18)
     (test-equal? "n is a negative number"
                  (arith -3)
                 -6)
     (test-equal? "n is 0"
                  (arith 0)
                 0)))

; tests for atoms?
(define atom?-tests
  (test-suite
   "atom?"

   (test-true "number"
              (atom? 3))
   (test-true "boolean"
              (atom? #f))
   (test-true "String"
              (atom? "String"))
   (test-true "Pi"
              (atom? pi))
   (test-false "list"
              (atom? '(1 2)))
   (test-false "null"
              (atom? null))
   (test-false "pair"
              (atom? (cons 5 10)))
   (test-false "empty list"
              (atom? '()))))


; tests for all-atoms?
(define all-atoms?-tests
  (test-suite
   "all-atoms?"
   (test-true "Empty list"
              (all-atoms? '()))
   (test-true "List with all atoms"
              (all-atoms? '(#\t "String" 6 pi)))
   (test-false "List with no atoms"
              (all-atoms? '(null '(1 2) (cons 5 3))))
   (test-false "A mixed list "
              (all-atoms? '( 5 "String" '(5 6) (cons 6 3))))))

; tests for not-all-atoms?
(define not-all-atoms?-tests
  (test-suite
   "not-all-atoms?"
   (test-false "Empty list"
              (not-all-atoms? '()))
   (test-false "List with one atom"
              (not-all-atoms? '(#\t)))
   (test-false "List with all atoms"
              (not-all-atoms? '(#\t "String" 6 pi)))
   (test-true "List with no atoms"
              (not-all-atoms? '(null '(1 2) (cons 5 3))))
   (test-true "A mixed list "
              (not-all-atoms? '( 5 "String" '(5 6) (cons 6 3))))))


; tests for all-ints?
(define all-ints?-tests
  (test-suite
   "all-ints?"
   (test-true "Empty List"
              (all-ints? '()))
   (test-true "List of all integers"
              (all-ints? '(2 4 6 7)))
   (test-true "List of one integers"
              (all-ints? '(2)))
   (test-false "List with no integer"
              (all-ints? '(null '(1 2) (cons 6 3))))
   (test-false "A mixed list"
              (all-ints? '(5 "String" '(5 6))))))


; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   length-tests
   arith-tests
   atom?-tests
   all-atoms?-tests
   not-all-atoms?-tests
   all-ints?-tests))

(run-tests all-tests)