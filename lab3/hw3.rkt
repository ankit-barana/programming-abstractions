#lang racket
; Ankit Barana

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

; 1. returns a list with first element of each list
(define (firsts lsts) (map first lsts))


; returns a list with all element (except first) of each list
(define (rests lsts) (map rest lsts))


; 2. returns the vector containing the sums of the corresponding elements of vec1 and vec2
(define (vec-add vec1 vec2)
  (if (empty? vec1)
      '()
      (map + vec1 vec2)))


; 3.  returns the dot product of the vectors
(define (dot-product vec1 vec2)
  (if (empty? vec1)
      0
      (apply + (map * vec1 vec2))))


; 4. returns a vector containing the dot product of vec with each row of mat
(define (mat-vec-mul mat vec)
  (if (empty? mat)
      '()
     (map (curry dot-product vec) mat)))


; 5. returns the transpose of the given matrix
(define (transpose mat)
  (if (or (empty? mat) (empty? (first mat)))
      '()
      (cons (firsts mat) (transpose (rests mat)))))


; 6. returns the product of matrix multiplication
(define (mat-mat-mul lhs rhs)
  (define (row-times-cols row mat)
    (map (lambda (col)
           (dot-product row col))
         (transpose mat)))

  (map (lambda (row)
       (row-times-cols row rhs))
       lhs))


; 7. returns a flat list with the same elements, in the same order as the given list
(define (flatten lst)
  (define (flatten-helper x)
    (if (list? x)
        (flatten x)
        (list x)))
  (apply append (map flatten-helper lst)))


; 8. sums all of the numbers in a list
(define (sum lst)
  (if (empty? lst)
      0
      (apply + (flatten lst))))
  

; 9. retuns a new list with procedure f applied to each element of the given list
(define (gen-map f lst)
  (map (lambda (x)
         (if (list? x)
             (gen-map f x)
             (f x)))
       lst))