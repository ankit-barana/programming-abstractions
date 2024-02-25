#lang racket
; Ankit Barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw1.rkt")

;1.  Define all-same? tests.
(define all-same?-tests
  (test-suite
   "all-same?"
   (test-true "Empty list"
                          (all-same? atom? '()))
   (test-true "List of elements that match the predicate"
                          (all-same? atom? '(#t 1 "String")))   
   (test-false "List of element not matching the predicate, except the beginning"
                          (all-same? integer? '(2 "String" 'foo)))   
   (test-true "even? on all even list"
                          (all-same? even? '(2 4 6 8)))   
   (test-false "List with all the elements not matching the predicate"
                          (all-same? list? '(#t 1 "String")))   
   (test-false "even? on non-even list"
                          (all-same? even? '(2 7 9 8)))))

;2. define make all tests
(define make-all-same-tests
  (test-suite
   "make-all-same"
   (test-true "Empty List"
                          ((make-all-same atom?) '()))
   (test-true "All elements satisfy the predicate"
                          ((make-all-same integer?) '(25 79 18)))
   (test-false "Only the first element satidfies the predicate"
                          ((make-all-same atom?) '(25 '() (cons) null)))
   (test-false "None satifies the predicate"
                          ((make-all-same list?) '(25 'foo 18))) ))

;3. define all-memebers?-test
(define all-members?-tests
  (test-suite
   "all-members?"
   (test-true "Empty Lists"
                          (all-members? '() '()))
   (test-false "Empty List 2"
                          (all-members? '(2 3 4) '()))
   (test-true "all integers present"
                          (all-members? '(2 4 6) '(6 7 4 8 5 74 2)))
   (test-true "all mixed-types present"
                          (all-members? '('foo 2 #f "String" '()) '('foo 2 #f "String" '())))
   (test-false "only some present"
                          (all-members? '('foo 2 #f 2 4 6) '(2 45 #f "String" 'foo)))
   (test-false "none present"
                          (all-members? '('foo 2 #f 2 4 6) '('symbol "String" #t 999)))))

;4. define delete-second test
(define delete-second-tests
  (test-suite
   "delete-second"
   (test-equal? "empty list"
                (delete-second 2 '())
                '())
   (test-equal? "both occurances are both at the start"
                (delete-second 2 '(2 2 3 4))
                '(2 3 4))
   (test-equal? "x does not exist in the list"
                (delete-second 2 '(4 5 6 8 6))
                '(4 5 6 8 6))
   (test-equal? "both occurances are in the end"
                (delete-second 2 '(5 6 7 2 2))
                '(5 6 7 2))
   (test-equal? "occurances are neither in end or start"
                (delete-second 2 '(5 2 7 2 2))
                '(5 2 7 2))))

;5. define delete-second-2 test
(define delete-second-2-tests
  (test-suite
   "delete-second-2"
   (test-equal? "empty list"
                (delete-second-2 '())
                '())
   (test-equal? "both occurances are both at the start"
                (delete-second-2 '(2 2 3 4))
                '(2 3 4))
   (test-equal? "x does not exist in the list"
                (delete-second-2 '(4 5 6 8 6))
                '(4 5 6 8 6))
   (test-equal? "both occurances are in the end"
                (delete-second-2 '(5 6 7 2 2))
                '(5 6 7 2))
   (test-equal? "occurances are neither in end or start"
                (delete-second-2 '(5 2 7 2 2))
                '(5 2 7 2))))

;6. define delete pair tests
(define delete-pair-tests
  (test-suite
   "delete pair"
   (test-equal? "empty list"
                (delete-pair 2 '())
                '())
   (test-equal? "list one item"
                (delete-pair 2 '(2))
                '(2))
   (test-equal? "list with all pairs - even"
                (delete-pair 2 '(2 2 2 2 2 2))
                '())
   (test-equal? "list with all pairs - odd"
                (delete-pair 2 '(2 2 2 2 2))
                '(2))
   (test-equal? "list with lists"
                (delete-pair 2 '((2 2) (3 2) 2))
                '(2))))

;7. define copy-test
(define copy-tests
  (test-suite
   "copy"
   (test-equal? "copy symbols 0 times"
                  (copy 3 'sym)
                  '(sym sym sym))
   (test-equal? "a number 0 times"
                  (copy 0 4)
                  '())
   (test-equal? "an empty list 3 times"
                  (copy 3 '())
                  '(() () ()))
   (test-equal? "a list of multiple numbers 3 times"
                  (copy 3 '(2 3))
                  '((2 3) (2 3) (2 3)))))


;8. define max-value test
(define max-value-tests
  (test-suite
   "max-value"
   (test-equal? "List with only one number"
                  (max-value '(1))
                1)
   (test-equal? "List with same number multiple times"
                (max-value '(3 3 3 3 3))
                3)
   (test-equal? "List with number in ascending order"
                (max-value '(1 2 3 4 5 6))
                6)
   (test-equal? "List with number in descending order"
                (max-value '(6 5 4 3 2 1))
                6)
    (test-equal? "List with numbers in descending order"
                (max-value '(6 5 4 3 2 1))
                6)
    (test-equal? "List with numbers in random order"
                (max-value '(9 6 3 6 2 25 1))
                25)))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   all-same?-tests
   make-all-same-tests
   all-members?-tests
   delete-second-tests
   delete-second-2-tests
   delete-pair-tests
   copy-tests
   max-value-tests))

(run-tests all-tests)
