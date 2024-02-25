#lang racket

; Provide the procedures for working with trees.
(provide tree make-tree empty-tree
         tree? empty-tree? leaf?
         tree-value tree-children)

; Provide 8 example trees.
(provide empty-tree T1 T2 T3 T4 T5 T6 T7 T8)

; Definition of tree datatype
(struct tree (value children) #:transparent)

; An empty tree is represented by null, just as an empty list is.
(define empty-tree null)

; (empty-tree? empty-tree) returns #t
(define empty-tree? null?)

; Convenience constructor
; (make-tree v c1 c2 ... cn) is equivalent to
; (tree v (list c1 c2 ... cn))
(define (make-tree value . children)
  (tree value children))

; Returns #t if the tree is a leaf. That is, if it has no children.
(define (leaf? t)
  (cond [(empty-tree? t) #f]
        [(not (tree? t)) (error 'leaf? "~s is not a tree" t)]
        [else (empty? (tree-children t))]))


; Example trees 
(define T1 (make-tree 50)) 
(define T2 (make-tree 22)) 
(define T3 (make-tree 10)) 
(define T4 (make-tree 5)) 
(define T5 (make-tree 17)) 
(define T6 (make-tree 73 T1 T2 T3)) 
(define T7 (make-tree 100 T4 T5)) 
(define T8 (make-tree 16 T6 T7))

;leaves! 
(define (leaves t)
  (letrec ([thelper (lambda (tree res)
                     ;shows you, with each call, what the value
                     ;of tree and res are
                     ;accompanies the diagram in the slides
                     (display tree)
                     (display "\n")
                     (display res)
                     (display "\n")
                     (display "~~~~~~~~~~~~~~~\n")
           (if (leaf? tree)
               (cons (tree-value tree) res)
               (foldl thelper res (tree-children tree))))])
    (thelper t empty)))