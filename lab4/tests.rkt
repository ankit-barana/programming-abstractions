#lang racket
; ankit barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw4.rkt")
(require "tree.rkt")


(define custom-tree (make-tree 20 (make-tree 10) (make-tree 15 (make-tree 5) (make-tree 8)))) ; I will be using the same tree in other tests as well
;            20
;           /  \  
;         10    15
;              /  \
;             5    8

; 1. replace-tests
(define replace-tests
  (test-suite
   "replace"
   (test-equal? "Empty list"
                (replace 1 'a '())
                '())
   (test-equal? "Replace single element"
                (replace 1 'a '(1 2 3))
                '(a 2 3))
   (test-equal? "No replacement needed"
                (replace 'x 'y '(1 2 3))
                '(1 2 3))
   (test-equal? "Replace multiple occurrences"
                (replace 2 'b '(1 2 3 2 2 4))
                '(1 b 3 b b 4))
   ))


; 2. sums the wight of bags
(define weigh-tests
  (test-suite
   "weigh"
   (test-equal? "Empty bags"
                (weigh '())
                0)
   (test-equal? "Single bag"
                (weigh '((duffle 8)))
                8)
   (test-equal? "Multiple bags"
                (weigh '((duffle 8) (garment-bag 1) (briefcase 200)))
                209)))


; 3.                                                                       ; check
(define heaviest-tests
  (test-suite
    "heaviest"
    (test-equal? "Empty bags"
                 (heaviest '())
                 '())
    (test-equal? "Single bag"
                 (heaviest '((duffle 8)))
                 'duffle)
    (test-equal? "Multiple bags"
             (heaviest '((duffle 8) (garment-bag 2) (briefcase 5) (valise 7) (steamer-trunk 65)))
             'steamer-trunk)))


; 4. returns the sum of subtrees of the given tree

(define child-sum-tests
  (test-suite
   "child-sum"
   (test-equal? "No immediate children"
                (child-sum (make-tree 7))
                0)
   (test-equal? "Single child"
                (child-sum (make-tree 5 (make-tree 3)))
                3)
   (test-equal? "Custom Tree"
                (child-sum custom-tree)
                25) ))

; 5. sum of all values
(define custom-all-sum-tests 
  (test-suite
   "all-sum"
   (test-equal? "No immediate children"
                (all-sum (make-tree 7))
                7) 
   (test-equal? "Single child"
                (all-sum (make-tree 5 (make-tree 3)))
                8)
   (test-equal? "Custom Tree"
                (all-sum custom-tree)
                58)))

; 6. apply f to all nodes
(define visit-tree-tests
  (test-suite
   "visit-tree"
   (test-equal? "Empty tree"
                 (visit-tree add1 empty-tree)
                 empty-tree)
   (test-equal? "Single tree"
                 (visit-tree add1 (make-tree 10))
                 (make-tree 11))))

; 7. size
(define size-of-tests
  (test-suite
    "size-of"
    (test-equal? "Empty tree"
      (size-of empty-tree)
      0)
    (test-equal? "tree with no children"
      (size-of (make-tree 5))
      1)
    (test-equal? "One child"
      (size-of (make-tree 5 (make-tree 3)))
      2)
    (test-equal? "Custom Tree"
      (size-of custom-tree)
      5) ))

; 8. height
(define height-tests
  (test-suite
   "height"
   (test-equal? "Empty tree"
                (height empty-tree)
                -1)
   (test-equal? "Tree with no children"
                (height (make-tree 5))
                0)
   (test-equal? "Tree with one child"
                (height (make-tree 5 (make-tree 3)))
                1)
   (test-equal? "Custom Tree"
                (height custom-tree)
                2) ))

; 9. pre-order traversal
(define pre-order-tests
  (test-suite
   "pre-order"
   (test-equal? "Empty tree"
                (pre-order empty-tree)
                '())
   (test-equal? "Tree with no children"
                (pre-order (make-tree 5))
                '(5))
   (test-equal? "Tree with one child"
                (pre-order (make-tree 5 (make-tree 3)))
                '(5 3))
   (test-equal? "Custom Tree"
                (pre-order custom-tree)
                '(20 10 15 5 8)) ))



; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   replace-tests
   weigh-tests
   heaviest-tests
   child-sum-tests
   custom-all-sum-tests
   visit-tree-tests
   size-of-tests
   height-tests
   pre-order-tests
   ))

(run-tests all-tests)
