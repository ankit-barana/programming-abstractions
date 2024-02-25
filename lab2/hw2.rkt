 #lang racket
; ankit barana

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt
(provide (all-defined-out))

; 1 - merges two sorted lists of numbers onto one sorted list
(define (merge lst1 lst2)
  (cond [(empty? lst1) lst2]
        [(empty? lst2) lst1]
        [(< (first lst1) (first lst2)) (cons (first lst1) (merge (rest lst1) lst2))]
        [else (cons (first lst2) (merge lst1 (rest lst2)))]))

; 2 - a sort function that uses min
(define (sort lst)
  (cond [(empty? lst) '()]
        [else (cons (apply min lst) (sort (remove (apply min lst) lst)))]))

; 3 - determines if sublist is present in the biglist
(define (has-sublist? sublist biglist)
  (cond [(empty? sublist) #t]
        [(> (length sublist) (length biglist)) #f]
        [(equal? sublist (take biglist (length sublist))) #t]
        [else (has-sublist? sublist (rest biglist))]))


; 4 - removes the first occurance of sublist from biglist
(define (delete-sublist sublist biglist)
  (cond [(empty? sublist) biglist]
        [(> (length sublist) (length biglist)) biglist]
        [(equal? sublist (take biglist (length sublist))) (helper biglist (length sublist))]
        [else (cons (first biglist)(delete-sublist sublist (rest biglist)))]))

  ; deletes the first n elements from list
  (define (helper list n)
    (if (= n 1)
        (rest list)
        (helper (rest list) (- n 1))))


; 5 - wraps a pair of parentheses around each top level element in given list
(define (nest lst)
  (cond [(empty? lst) lst]
        [(cons (list (first lst)) (nest (rest lst))) ]))


; 6 - finds the base 10 value of 1s and 0s
(define (eval-bin lst)
  (define (bin-to-num lst num)
    (if (empty? lst)
        num
        (bin-to-num (rest lst) (+ (* 2 num) (first lst)))))
  (bin-to-num lst 0))


; 7 - replaces each instance of old in the lst with new
(define (exchange old new lst)
  (cond [(empty? lst) '()]
        [(equal? old (first lst)) (cons new (exchange old new (rest lst)))]
        [else (cons (first lst) (exchange old new (rest lst)))]))

; 8 - replaces old-lst items in lst with those of new-list
(define (all-exchange old-lst new-lst lst)
  (cond
    [(or (empty? old-lst) (empty? new-lst) (empty? lst)) lst] ; 
    [(> (length old-lst) (length lst)) lst]
    [(equal? old-lst (take lst (length old-lst))) (append new-lst (all-exchange old-lst new-lst (all-exchange-helper lst (length old-lst))))]
    [else (cons (first lst) (all-exchange old-lst new-lst (rest lst)))]))

  ; deletes the first n elements of the list
  (define (all-exchange-helper list n)
    (if (= n 1)
        (rest list)
        (helper (rest list) (- n 1))))


