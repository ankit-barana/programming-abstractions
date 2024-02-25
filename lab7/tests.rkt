#lang racket
; Ankit Barana & Loki Fondeur

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw7.rkt")

(define infinite-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z infinite-stream))))

(define finite-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z (stream-cons 'a (stream-cons 'b (stream-cons 'z empty-stream)))))))

(define stream-remove-all-tests
  (test-suite
   "stream-remove"
   (test-equal? "infinite stream"
                (stream->list (stream-take (stream-remove-all 'x infinite-stream) 10))
                '(y z y z y z y z y z))
   (test-equal? "infinite stream (element is not in the stream)"
                (stream->list (stream-take (stream-remove-all 3 infinite-stream) 10))
                '(x y z x y z x y z x))
   (test-equal? "finite stream"
                (stream->list (stream-remove-all 'z finite-stream))
                '(x y a b))
                
   ))

(define stream-replace-tests
  (test-suite
   "stream-remove"
   (test-equal? "inifinite stream"
                (stream->list (stream-take (stream-replace 'x 'catfish infinite-stream) 10))
                '(catfish y z catfish y z catfish y z catfish))
   (test-equal? "inifinite stream (+ remove) (element is not in stream)"
                (stream->list (stream-take (stream-replace 'y 'z (stream-remove-all 'y infinite-stream)) 10))
                '(x z x z x z x z x z))
   (test-equal? "finite stream"
                (stream->list (stream-replace 'z 's finite-stream))
                '(x y s a b s))
   ))

(define grune-a-b-tests
  (test-suite
   "grune-a-b tests"
   (test-equal? "empty stream"
              (grune-a-b empty-stream)
              empty-stream)

   (test-equal? "stream with one element"
                (stream->list (grune-a-b '(a)))
                '(a))

   (test-equal? "stream with no a's"
                (stream->list (grune-a-b '(c c d c b)))
                '(c c d c b))


   (test-equal? "stream with 'a' with other elements"
                (stream->list (grune-a-b '(b a c)))
                '(b a c))

   (test-equal? "stream with 2 a's"
                (stream->list (grune-a-b '(a a)))
                '(b))

   (test-equal? "stream with 2 a's in the beginning"
                (stream->list (grune-a-b '(a a c)))
                '(b c))

   (test-equal? "stream with 2 a's in the middle"
                (stream->list (grune-a-b '(c d a a c)))
                '(c d b c))

   (test-equal? "stream with 2 a's in the end"
                (stream->list (grune-a-b '(d c a a)))
                '(d c b))

   (test-equal? "stream with non-consecutive 2 a's"
                (stream->list (grune-a-b '(a d a c)))
                '(a d a c))

   (test-equal? "stream with multiple occurances of 2 a's"
                (stream->list (grune-a-b '(a b c d a a a b a a)))
                '(a b c d b a b b))
   ))


(define better-grune-a-b-tests
  (test-suite
   "better-grune-a-b tests"
   (test-equal? "empty stream"
              (better-grune-a-b empty-stream)
              empty-stream)

   (test-equal? "stream with one element"
                (stream->list (better-grune-a-b '(a)))
                '(a))

   (test-equal? "stream with no a's"
                (stream->list (better-grune-a-b '(c c d c b)))
                '(c c d c b))


   (test-equal? "stream with 'a' with other elements"
                (stream->list (better-grune-a-b '(b a c)))
                '(b a c))

   (test-equal? "stream with 2 a's"
                (stream->list (better-grune-a-b '(a a)))
                '(b))

   (test-equal? "stream with 2 a's in the beginning"
                (stream->list (better-grune-a-b '(a a c)))
                '(b c))

   (test-equal? "stream with 2 a's in the middle"
                (stream->list (better-grune-a-b '(c d a a c)))
                '(c d b c))

   (test-equal? "stream with 2 a's in the end"
                (stream->list (better-grune-a-b '(d c a a)))
                '(d c b))

   (test-equal? "stream with non-consecutive 2 a's"
                (stream->list (better-grune-a-b '(a d a c)))
                '(a d a c))

   (test-equal? "stream with multiple occurances of 2 a's"
                (stream->list (better-grune-a-b '(a b c d a a a b a a)))
                '(a b c d b a b b))
   ))



(define grune-tests
  (test-suite
   "grune tests"
   (test-equal? "empty stream"
              ((grune 'a 'b) empty-stream)
              empty-stream)

   (test-equal? "stream with one element"
                (stream->list ((grune 'a 'b) '(a)))
                '(a))

   (test-equal? "stream with no a's"
                (stream->list ((grune 'a 'b) '(c c d c b)))
                '(c c d c b))

   (test-equal? "stream with 'a' with other elements"
                (stream->list ((grune 'a 'b) '(b a c)))
                '(b a c))

   (test-equal? "stream with 2 a's"
                (stream->list ((grune 'a 'b) '(a a)))
                '(b))

   (test-equal? "stream with 2 a's in the beginning"
                (stream->list ((grune 'a 'b) '(a a c)))
                '(b c))

   (test-equal? "stream with 2 a's in the middle"
                (stream->list ((grune 'a 'b) '(c d a a c)))
                '(c d b c))

   (test-equal? "stream with 2 a's in the end"
                (stream->list ((grune 'a 'b) '(d c a a)))
                '(d c b))

   (test-equal? "stream with non-consecutive 2 a's"
                (stream->list ((grune 'a 'b) '(a d a c)))
                '(a d a c))

   (test-equal? "stream with multiple occurances of 2 a's"
                (stream->list ((grune 'a 'b) '(a b c d a a a b a a)))
                '(a b c d b a b b))

   (test-equal? "stream with nested grunes"
                (stream->list ((grune 'b 'c) ((grune 'a 'b) '(a b c d a a a b a a))))
                '(a b c d b a c))
   ))




; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   stream-remove-all-tests
   stream-replace-tests
   grune-a-b-tests
   better-grune-a-b-tests
   grune-tests
   ))

(run-tests all-tests)
