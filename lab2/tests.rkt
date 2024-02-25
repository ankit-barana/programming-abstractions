#lang racket
; ankit barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw2.rkt")


; 1 - merges two sorted lists of numbers onto one sorted list
(define merge-tests
  (test-suite
   "merge"
   (test-equal? "Odds and evens"
                (merge '(1 3 5 7 9) '(2 4 6 8 10))
                '(1 2 3 4 5 6 7 8 9 10))
   (test-equal? "Same lists"
                (merge '(2 3 4 5) '(2 3 4 5))
                '(2 2 3 3 4 4 5 5))
   (test-equal? "An empty and a non-empty list"
                (merge '() '(2 4 6 8 10))
                '(2 4 6 8 10))
   (test-equal? "Continued list"
                (merge '(1 2) '(3 4 5 6))
                '(1 2 3 4 5 6))
   (test-equal? "Two empty lists"
                (merge null null)
                null)))
; 2 - a sort function that uses min
(define sort-tests
  (test-suite
   "sort"
   (test-equal? "Empty list"
                (sort '()) '())
   (test-equal? "Single element"
                (sort '(2)) '(2))
   (test-equal? "Sorted list"
                (sort '(1 2 3 4)) '(1 2 3 4))
   (test-equal? "descendingly sorted list"
                (sort '(5 4 3 2)) '(2 3 4 5))
   (test-equal? "Random list"
                (sort '(5 3 1 8 -11 3 7)) '(-11 1 3 3 5 7 8))))

; 3 - determines if sublist is present in the biglist
(define has-sublist?-tests
  (test-suite
   "has-sublist?"
   (test-equal? "Empty sub-list and list"
                (has-sublist? '() '()) #t)
   (test-equal? "Empty sub-list"
                (has-sublist? '() '(2 3 4)) #t)
   (test-equal? "Sublist in empty list"
                (has-sublist? '(2 3 4) '()) #f)
   (test-equal? "Single element present"
                (has-sublist? '(2) '(2 3)) #t)
   (test-equal? "Single element Not Present"
                (has-sublist? '(2) '(3)) #f)
   (test-equal? "Sublist at the beginning"
                (has-sublist? '(2 3) '(2 3 4 5)) #t)
   (test-equal? "elements are present at the end"
                (has-sublist? '(2 2 3) '(4 5 2 2 3)) #t)
   (test-equal? "elements not present continuously"
                (has-sublist? '(2 3 4) '(1 2 5 3 4)) #f) ))

; 4 - removes the first occurance of sublist from biglist
(define delete-sublist-tests
  (test-suite
   "delete-sublist"
   (test-equal? "Empty list"
                (delete-sublist '() '()) '())
   (test-equal? "Sublist is present"
                (delete-sublist '(2 5) '(9 2 5 6 3)) '(9 6 3))
   (test-equal? "Sublist is not present"
                (delete-sublist '(2 5) '(9 7 8 6 3)) '(9 7 8 6 3))
   (test-equal? "Sublist at the beginning"
                (delete-sublist '(2 3) '(2 3 4 5)) '(4 5))
   (test-equal? "elements are present at the end"
                (delete-sublist '(2 2 3) '(4 5 2 2 3)) '(4 5))
   (test-equal? "elements not present continuously"
                (delete-sublist '(2 3 4) '(1 2 5 3 4)) '(1 2 5 3 4)) ))

; 5 - wraps a pair of parentheses around each top level element in given list
(define nest-tests
  (test-suite
   "nest"
   (test-equal? "Empty list"
                (nest '()) '())
   (test-equal? "Single element"
                (nest '(2)) '((2)))
   (test-equal? "List with atoms"
                (nest '(a #t c)) '((a) (#t) (c)))
   (test-equal? "List with nest list"
                (nest '(a 2 #f (b (c d)) e)) '((a) (2) (#f) ((b (c d))) (e))) ))

; 6 - finds the base 10 value of 1s and 0s
(define eval-bin-tests
  (test-suite
   "eval-bin"
   (test-equal? "Empty list"
                (eval-bin '()) 0)
   (test-equal? "Single element 1"
                (eval-bin '(1)) 1)
   (test-equal? "Single element 0"
                (eval-bin '(0)) 0)
   (test-equal? "Evaluates to 7"   ; used coolconversion.com to get the binary digits 
                (eval-bin '(1 1 1)) 7)
   (test-equal? "Evaluates to 181"
                (eval-bin '(1 0 1 1 0 1 0 1)) 181)))

; 7 - replaces each instance of old in the lst with new
(define exchange-tests
  (test-suite
   "exchange"
      (test-equal? "Empty list"
                (exchange 2 3 '())
                '())
   (test-equal? "Replace single element"
                (exchange 2 3 '(2 4 5))
                '(3 4 5))
   (test-equal? "Replace multiple elements"
                (exchange 2 3 '(3 5 2 5 2 2 4 2 5))
                '(3 5 3 5 3 3 4 3 5))
   (test-equal? "No replacements"
                (exchange 2 3 '(5 6 7 8))
                '(5 6 7 8))
   (test-equal? "Replace at beginning and end"
                (exchange 2 3 '(2 4 5 2))
                '(3 4 5 3))))

; 8 - replaces old-lst items in lst with those of new-list
(define all-exchange-tests
  (test-suite
   "all-exchange"
   (test-equal? "Replacing a single element"
                (all-exchange '(s) '(n) '(s a m a s))
                '(n a m a n)) 
   (test-equal? "Replacing multiple elements in the begining "
                (all-exchange '(b o) '(m u) '(b o b))
                '(m u b))
   (test-equal? "Case 3: Empty list"
                (all-exchange '() '() '())
                '())))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   merge-tests
   sort-tests
   has-sublist?-tests
   delete-sublist-tests
   nest-tests
   eval-bin-tests
   exchange-tests
   all-exchange-tests))

(run-tests all-tests)
