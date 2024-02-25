#lang racket
; Ankit Barana

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

(define len
  (lambda (lst)
    (cond [(empty? lst) 0]
          [else (+ 1 (len (rest lst)))])))

#|
If n is greater than or equal to 10 and even, returns n + 2
If n is greater than or equal to 10 and odd, returns n
If n is less than 10 and even, returns n * 5
If n is less than 10 and odd, returns n + n
|#

(define (arith n)
  (cond [(and (>= n 10) (even? n)) (+ n 2)]
        [(and (>= n 10) (odd? n)) n]
        [(and (< n 10) (even? n)) (* n 5)]
        [(and (< n 10) (odd? n)) (+ n n)]
        [else "Not a valid input"]))


; checks if the input is an atom
; returns true if it is, false otherwise
(define (atom? item)
  (and (not (pair? item))
       (not (null? item))))

; checks if the input is a list of atoms
; returns true if the list is either empty or a list of only atoms, false otherwise
(define (all-atoms? lst)
  (if (empty? lst)    
      #t     
      (if (atom? (first lst))
          (all-atoms? (rest lst))
          #f)))

; checks if the input is not a list of atoms
; returns true if the list is not a list of only atoms or empty, false if it is either
(define (not-all-atoms? lst)
  (cond [(empty? lst) #f]   ; in the lab instructions, it is mentioned that it is the oppostive of all-atoms (with an exmaple code), so I made it return #f if the list is empty.
        [else (cond [(not (atom? (first lst))) #t]
                    [else (not-all-atoms? (rest lst))])]))


; checks if the input is a list of integers
; returns true if the list is either empty or a list of only integers, false otherwise
(define (all-ints? lst)
  (if (empty? lst)    
      #t     
      (if (integer? (first lst))
          (all-ints? (rest lst))
          #f))) 






  