#lang racket
; Ankit Barana & Loki Fondeur 

(require "keyboard.rkt")
(require racket/stream)

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

(define (stream-remove-all x s)
  (cond [(stream-empty? s) empty-stream]
        [(equal? (stream-first s) x) (stream-remove-all x (stream-rest s))]
        [else (stream-cons (stream-first s) (stream-remove-all x (stream-rest s)))] 
        ))

(define (stream-replace x y s)
  (cond [(stream-empty? s) empty-stream]
        [(equal? (stream-first s) x) (stream-cons y (stream-replace x y (stream-rest s)))]
        [else (stream-cons (stream-first s) (stream-replace x y (stream-rest s)))]
        ))

(define (grune-a-b s)
  (cond [(stream-empty? s) empty-stream]
        [(equal? (stream-length s) 1) s]
        [(and (equal? (stream-first s) 'a) (equal? (stream-first (stream-rest s)) 'a)) (stream-cons
                                                                                       'b (grune-a-b (stream-rest (stream-rest s))))]
        [else (stream-cons (stream-first s) (grune-a-b (stream-rest s)))]))

(define (grune a b)
  (lambda (s)
    (cond [(stream-empty? s) empty-stream]
          [(equal? (stream-length s) 1) s]
          [(and (equal? (stream-first s) a) (equal? (stream-first (stream-rest s)) a))
           (stream-cons b ((grune a b) (stream-rest (stream-rest s))))]
          [else (stream-cons (stream-first s) ((grune a b) (stream-rest s)))])))

(define better-grune-a-b (grune 'a 'b))