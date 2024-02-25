#lang racket
; ankit barana

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))
(require "tree.rkt")


; 1. replaces every occurance of a with b
(define (replace a b lst)
  (foldr (lambda (head  result)  ; combine
           (if (equal? a head)
               (cons b result)
               (cons head result)))
         empty ; base-case
         lst)) ; argument

; 2. sums up the weight of each bag
(define (weigh lst)
  (foldr + 0 (map second lst)))

; 3. returns the name of the heaviest weight
(define (heaviest bags)
  (if (empty? bags) '()
  (car (foldl (lambda (bag1 bag2)
                (if (> (second bag1) (second bag2))
                    bag1
                    bag2))
              (first bags)
              bags))))



; 4. returns the sum of all immediate children 
(define (child-sum t)
  (cond
    [(empty-tree? t) 0] 
    [else
      (apply + 
             (map tree-value (tree-children t)))]))



; 5. returns the sum of all the values in the tree
(define (all-sum t)
  (cond
    [(empty-tree? t) 0] 
    [else
      (+ (tree-value t) 
         (apply + 
                (map all-sum (tree-children t))))]))

; 6. applies f to every node

(define (visit-tree f tree)
 (cond [(empty-tree? tree) empty-tree]
       [(leaf? tree) (make-tree (f (tree-value tree)))]
       [else (make-tree (f (tree-value tree)) (map (lambda (child) (visit-tree f child)) (tree-children tree)))]))


; 7. returns the total number of nodes in a tree
(define (size-of t)
  (cond
    [(empty-tree? t) 0] 
    [else
      (+ 1 
         (foldl + 0 (map size-of (tree-children t))))]))
 

; 8. returns the height of tree. Empty tree has a height of -1 
(define (height t)
  (cond
    [(empty-tree? t) -1] 
    [else
      (if (leaf? t) 
          0
          (add1 (apply max
                         (map height (tree-children t)))))]))

; 9. returns a pre-order traversal of tree
(define (pre-order tree)
  (if (empty-tree? tree)
      '()
      (cons (tree-value tree)
            (apply append (map pre-order (tree-children tree))))))

