#lang racket
; Ankit Barana

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

; checks if the input is an atom
(define (atom? item)
  (and (not (pair? item))
       (not (null? item))))

;1.  checks if all the the elements in a list meet the condition specified by the predicate.
; returns true if the list is empty or if every element in it satisfies the predicate.
(define (all-same? predicate lst)
  (if (empty? lst)
      #t
      (if (predicate (first lst))
          (all-same? predicate (rest lst))
          #f)))


;2.  takes a predicate and returns a procedure
; the returning procedure takes an argument and returns true for the empty lists or a list for which the predicate returns #t for all elements
(define (make-all-same predicate)
  (lambda (lst)
    (if (empty? lst)
        #t
        (if (predicate (first lst))
            ((make-all-same predicate) (rest lst))
            #f))))


;3. all-memebers? helper
(define (contains item lst)
  (cond [(empty? lst) #f]
        [(equal? (first lst) item) #t]
        [else (contains item (rest lst))]))


;3.  returns #t if every member of lst1 is also a member of lst2
; #f otherwise
(define (all-members? lst1 lst2)
  (if (empty? lst1)
      #t
      (if (contains (first lst1) lst2)
          (all-members? (rest lst1) lst2 )
          #f)))


;4. removes the second occurrence of x from lst,
(define (delete-second x lst)
  (if (null? lst)
      '()
      (if (equal? x (first lst))
                  (cons x (remove x (rest lst))) 
                  (cons (first lst) (delete-second x (rest lst))))))

;5. same as 4, but with curring
(define (delete-second-2 lst)
    ((curry-delete-second 2) lst))

;5. curry-delete-second
(define (curry-delete-second x)
  (lambda (lst)
    (if (null? lst)
      '()
      (if (equal? x (first lst))
                  (cons x (remove x (rest lst))) 
                  (cons (first lst) (delete-second x (rest lst)))))))

;6. deletes the every pair of x 
(define (delete-pair x lst)
  (cond [(null? lst) '()]
        [(null? (rest lst)) (list (first lst))]
        [else  (if (equal? x (first lst))
             (if (equal? (first lst) (second lst))
                  (delete-pair x (rest (rest lst)))
                  (cons (first lst) (delete-pair x (rest lst))))
             (delete-pair x (rest lst)))]))

;7. returns a list containing n copies of the given element
(define (copy n data)
  (if (equal? n 0)
      (list)
      (cons data (copy (- n 1) data))))


;8. returns the maximum value in a non-empty list
(define (max-value lst)
  (if (null? (rest lst))
      (first lst)
      (if (> (first lst) (max-value (rest lst)))
          (first lst)
          (max-value (rest lst)))))

