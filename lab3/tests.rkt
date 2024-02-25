#lang racket
; Ankit Barana

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw3.rkt")

; 1. tests firsts
(define firsts-tests
  (test-suite
   "firsts"
   (test-equal? "Single list"      ; we dont need empty test as we are only handling non-empty equal length lists
                (firsts '((1 2 3)))
                '(1))
   (test-equal? "Single list with single element"      
                (firsts '((1)))
                '(1))
   (test-equal? "Multiple lists"
                (firsts '((1 2 3) (4 5 6) (7 8 9)))
                '(1 4 7))
   (test-equal? "Nested lists "
                (firsts '((1 (2 3)) ((4 5) 6 7) (3 (10 23 44) 5)))
                '(1 (4 5) 3))))


; 1. tests rests
(define rests-tests
  (test-suite
   "rests"
   (test-equal? "Single list"      
                (rests '((1 2 3)))
                '((2 3)))
   (test-equal? "Single list with single element"      
                (rests '((1)))
                '(()))
   (test-equal? "Multiple lists"
                (rests '((1 2 3) (4 5 6) (7 8 9)))
                '((2 3) (5 6) (8 9)))
   (test-equal? "Nested lists"
                (rests '((1 (2 3)) ((4 5) 6 7) (3 (10 23 44) 5)))
                '(((2 3)) (6 7) ((10 23 44) 5))) ))


; 2. tests vec-add
(define vec-add-tests
  (test-suite
   "vec-add"
   (test-equal? "Empty vector"      
                (vec-add '() '())
                '())
   (test-equal? "Single element vector"      
                (vec-add '(2) '(5))
                '(7))
   (test-equal? "Positive only vector"      
                (vec-add '(1 2 3) '(3 2 1))
                '(4 4 4))
   (test-equal? "Negative only vector"      
                (vec-add '(-1 -2 -3) '(-3 -2 -1))
                '(-4 -4 -4))
   (test-equal? "Mixed vector"      
                (vec-add '(-1 2 -3) '(3 -2 1))
                '(2 0 -2)) ))


; 3. tests dot-product
(define dot-product-tests
  (test-suite
   "dot-product"
   (test-equal? "Empty vectors"      
                (dot-product '() '())
                0)
   (test-equal? "Single element vectors"      
                (dot-product '(2) '(4))
                8)
   (test-equal? "Positives only vectors"      
                (dot-product '(1 5 1) '(2 1 1))
                8)
   (test-equal? "Negatives only vectors"      
                (dot-product '(-1 -5 -4) '(-2 -3 -2))
                25) 
   (test-equal? "Mixed vectors"      
                (dot-product '(-1 5 -4) '(-2 -3 2))
                -21)))


; 4. tests mat-vec-mul
(define mat-vec-mul-tests
  (test-suite
   "mat-vec-mul"
   (test-equal? "Empty"      
                (mat-vec-mul '() '())
                '())
   (test-equal? "Single element matrix"      
                (mat-vec-mul '((2)) '(3))
                '(6))
   (test-equal? "Single row matrix with multiple elements"
                (mat-vec-mul '((2 3 4)) '(3 3 6))
                '(39))
   (test-equal? "Multiple row matrix"
                (mat-vec-mul '((1 4 7) (2 5 8) (3 6 9)) '(1 2 3))
                '(30 36 42)) ))


; 5. tests transpose
(define transpose-tests
  (test-suite
   "transpose"
   (test-equal? "Empty matrix"      
                (transpose '())
                '())  
   (test-equal? "single row matrix"      
                (transpose '((1 2 3 4)))
                '((1) (2) (3) (4))) 
   (test-equal? "single column matrix"      
                (transpose '((1) (2) (3) (4)))
                '((1 2 3 4)))
   (test-equal? "multiple row matrix"      
                (transpose '((1 4 7) (2 5 8) (3 6 9)))
                '((1 2 3) (4 5 6) (7 8 9)))
  (test-equal? "another multiple row matrix"      
                (transpose '((1 2 3) (4 5 6)))
                '((1 4) (2 5) (3 6))) ))

; 6. tests mat-mat-mul
(define mat-mat-mul-tests
  (test-suite
   "mat-mat-mul"
   (test-equal? "2x2 matrices"
                (mat-mat-mul '((1 2) (3 4)) '((2 0) (1 1)))
                '((4 2) (10 4)))
   (test-equal? "2x3 and 3x2 matrices"
                (mat-mat-mul '((1 2 3) (4 5 6)) '((2 0) (1 1) (0 2)))
                '((4 8) (13 17)))
   (test-equal? "3x3 and 3x3 matrices"
                (mat-mat-mul '((1 2 3) (4 5 6) (7 8 9)) '((1 0 1) (0 1 0) (1 0 1)))
                '((4 2 4) (10 5 10) (16 8 16)))
   (test-equal? "with an empty matrix"
                (mat-mat-mul '() '((1 2) (3 4)))
                '())
   (test-equal? "within empty matrices"
                (mat-mat-mul '() '())
                '())))


; 7. tests flatten
(define flatten-tests
  (test-suite
   "flatten"
   (test-equal? "Empty list"      
                (flatten '())
                '())
   (test-equal? "Single list"      
                (flatten '(1 2 3))
                '(1 2 3))
   (test-equal? "A list in A list"      
                (flatten '((1 2 3)))
                '(1 2 3))
   (test-equal? "Nested lists with unnested objects"      
                (flatten '((2 3 4) 2 11 (6 8 4)))
                '(2 3 4 2 11 6 8 4))
   (test-equal? "Nested lists Only"      
                (flatten '((2 3 4) (6 8 4)))
                '(2 3 4 6 8 4)) ))


; 8. tests sum (of all elementso of a not necessarily flattened list)
(define sum-tests
  (test-suite
   "sum"
   (test-equal? "Empty list"      
                (sum '())
                0)
   (test-equal? "Single list"      
                (sum '(1 2 3))
                6)
   (test-equal? "A list in A list"      
                (sum '((1 2 3)))
                6)
   (test-equal? "Nested lists with unnested objects"      
                (sum '((2 3 4) 2 11 (6 8 4)))
                40)
   (test-equal? "Nested lists Only"      
                (sum '((2 3 4) (6 8 4)))
                27) ))


;; 9. tests gen-map (of all elementso of a not necessarily flattened list)
(define gen-map-tests
  (test-suite
   "gen-map"
   (test-equal? "Empty list"      
                (gen-map add1 '())
                '())
   (test-equal? "adds 1 to each element"      
                (gen-map add1 '(3 (4 5)))
                '(4 (5 6)))
   (test-equal? "Doubles each element"      
                (gen-map (lambda (x) (* x 2)) '(1 2 3))
                '(2 4 6)) 
   (test-equal? "nests each element"      
                (gen-map (lambda (x) (list x)) '(3 (4 5)))
                '((3) ((4) (5))))))


; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   firsts-tests
   rests-tests
   vec-add-tests
   dot-product-tests
   mat-vec-mul-tests
   transpose-tests
   mat-mat-mul-tests
   flatten-tests
   sum-tests
   gen-map-tests
   ))

(run-tests all-tests)
