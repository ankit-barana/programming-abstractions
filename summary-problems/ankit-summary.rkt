#lang racket

; Your Name Here: Ankit Barana
; Date of Last Update: 11/17/2023

(require rackunit rackunit/text-ui)

; Each completed problem earns you 10 points. Correct adherence to the
; policies and submission earns you an additional 10 points, for a total
; of 100 points. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM 1.
; TIMELINE: Anytime.

; Write (location-of lst x) which returns the 0-based index of the first
; occurrence of x in lst (use equal? to compare elements). If x is
; not in the list, return #f. (Notice that in this procedure, the
; list is the first parameter.) While location-of is a built-in Racket
; procedure, you are required to write your own implementation and tests.
; You may *not* use anything more complex than basic list and control
; flow operations. 

; As examples:
; (location-of '(x y z z y) 'x) returns 0
; (location-of '(x y z z y) 'y) returns 1
; (location-of '(x y z z y) 'a) returns #f
; (location-of empty 'x) returns #f

;==> past attemp, no new attempt
(define (location-of lst x)
  (define (helper lst index)  
    (cond [(empty? lst) #f]
          [(equal? (first lst) x) index]
          [else (helper (rest lst) (+ index 1))]))
  (helper lst 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM 2.
; TIMELINE: On or after Week 3. 


; Use a fold to write (loc x lst) which returns the 0-based index of x in lst
; or -1 if x is not an element of lst.
; It should return the index of the *FIRST* instance of x in lst
; (see the first test below).

;==> new attempt
(define (loc x lst)
  (foldl (lambda (item index)
           (define (helper lst index)
             (cond [(empty? lst) -1]
                   [(equal? (first lst) x) index]
                   [else (helper (rest lst) (+ index 1))]))
           (helper lst 0))
         -1
         lst))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM 3.
; TIMELINE: On or after Week 4.

; Write (is-in? a lst) that returns #t if a is an element of
; the (not necessarily flat) list lst and #f otherwise.

; Example calls: 
; (is-in? 3 '(2 1 (4 2 (5 3) 1))) returns #t
; (is-in? 'x '(a (b c (d)) e f)) returns #f

;==> past attemp, no new attempt
(define (is-in? a lst)
  (cond [(empty? lst) #f]
        [(equal? (first lst) a) #t]
        [(list? (first lst)) (or (is-in? a (first lst)) (is-in? a (rest lst)))]
        [else (is-in? a (rest lst))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEM 4.
; TIMELINE: On or after Week 3 (convert-all) / Week 4 (c-a).

; We have a list of (running) road races that can each be represented by a
; three-element list (name length metric). For instance ("Boston 10K" 10 "km")
; represents a race called the "Boston 10K" that is 10 kilometers long. Most
; road races in the US are in kilometer ("km") or mile ("mi") distances -
; assume that those are the only distance measures in this problem.

; Write *TWO* procedures, both of which have the following functionality:
; they take two arguments, measure and lst, where lst is a list of races as
; described above and measure is the string "mi" or "km". They should
; return a list of distances, in the order of the provided races, where the
; length has been converted to the provided metric type, measure.

; Procedure one should be called convert-all and should use *NO HIGHER ORDER*
; functionality.

; Procedure two should be called c-a and *MUST USE SOME HIGHER ORDER*
; functionality.

; If we have races as below: 
(define races
  '(("Boston 10K" 10 "km")
    ("Portland Mile" 1 "mi")
    ("Cleveland 100" 100 "mi")
    ("Oberlin 5K" 5 "km")))
; then
; (convert-all "mi" races) gives all the race lengths in miles:'(6.2 1 100 3.1)
; (convert-all "km" races) gives all the race lengths in kilometers:'(10 1.61 161.0 5) 

; Note that an integer multipled by a real number produces a real number
; (e.g. (* 5 2.0) is 10.0) in Racket. It is OK if your answer produces the
; the mix of integers and real numbers shown in the above examples.

; For your reference: 1 mile is 1.61 kilometers and 1 kilometer is 0.62 miles

; without higher order functionality
;==> past attemp, no new attempt
(define (convert-all measure lst)
  (cond [(empty? lst)empty] ; returns an empty list of the input is empty
        [(not (equal? 3 (length (first lst)))) (error 'convert-all "Invalid format of race(s)")] ; gives error if the races are in wrong format
        [(equal? measure "mi") (cond [(equal? (third (first lst)) "mi")  ; if we are converting into miles 
                                      (cons (second (first lst)) (convert-all measure (rest lst)))]  ; and distance is in miles, we dont need to perform amy operation on it
                                     [else (cons (* (second (first lst)) 0.62) (convert-all measure (rest lst)))])]  ; otherwise, distance is in kms, we need to convert it to miles.
        
        [else (cond [(equal? (third (first lst)) "km")
                                      (cons (second (first lst)) (convert-all measure (rest lst)))]
                    [else (cons (* (second (first lst)) 1.61) (convert-all measure (rest lst)))])]
        ))

;==> past attemp, no new attempt
; with higher order functionality 
(define (c-a measure lst)
  (if (equal? measure "mi")
      (map (lambda (race)
             (if (not (equal? 3 (length race)))
                   (error 'c-a "Invalid format of race(s")
             (let ([unit (third race)]
                   [distance (second race)])
               (if (equal? unit "mi") distance (* distance 0.62)))))
           lst)
      (map (lambda (race)
             (if (not (equal? 3 (length race)))
                   (error 'c-a "Invalid format of race(s")
             (let ([unit (third race)]
                   [distance (second race)])
               (if (equal? unit "km") distance (* distance 1.61)))))
           lst)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEMS 6 & 7.
; TIMELINE: On or after Week 10.

; The goal of this problem is to add a new Racket special form to your MiniScheme
; implementation. Given the description below, you should edit your parse.rkt and
; interp.rkt accordingly (no tests necessary to submit, although highly recommended)
; and submit them. You do not have to be done with Homeworks 6 & 8 to submit
; this summary problem, although you should have a completed Homework 5. 

; Consider a special form called select. The select form takes a value expression
; followed by 1 or more 2-element lists of the form [num exp], where num is a number
; literal and exp is an arbitrary expression. The behavior of select is that the
; value expression is evaluated first and then compared to each of the number
; literals in order. If there is a match, then the corresponding expression is
; evaluated and its value is the value of the whole select.

; For example,
; (select (+ x y)
;         [1 (* a b)]
;         [2 (- 1 2)]
;         [3 'a])

; first evaluates (+ x y). If the value is 2, the value of select is -1. If the value of
; (+ x y) is 3, the value of select is 'a, etc. If the value of (+ x y) is not 1, 2, or 3,
; this∂ should through an error with the message "select value didn't match any cases."

; Add a constructor to parse.rkt for a select-exp expression, where you can access
; the value, cases (in the above example, 1, 2 and 3 are the cases), and expressions.
; Then implement support for select in both parse and eval-exp.

; STRUCT
(struct select-exp (value cases expressions) #:transparent)

; PARSER
(define (parse input)
  (cond ((list? input)
         (cond [(eq? (first input) 'select) (if (= (length input) 3)
                                             (select-exp (parse (second input))
                                                         (map (lambda (pair) (cons (parse (car pair)) (parse (cdr pair)))) (rest input))
                                                         (map parse (rest input)))
                                             ((error 'parse "Invalid syntax ~s" input) input))]))))



; NOTE HERE WHETHER YOU HAVE DONE THIS FOR YOUR OWN RECORDS!: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PROBLEMS 8 & 9.
; TIMELINE: On or after Week 10.

; Here you'll demonstrate your ability to write a complete test suite, given 
; existing procedures. 

; (1) Write an extensive test suite for each procedure. Your grade depends on
;     how well each individual test covers a different case for the problem.
;     You may want to write a comment explaining what scenario the test covers to
;     aid grading. To earn full credit, you shoudl consider tests for correct
;     AND incorrect input (i.e. test-exn should appear!)


; (2) Based on your tests, explain why the procedure does (or does not) correctly
;     implement the specifications as given.

; Procedure 1: opposite
; This procedure should take a logical operator op and return a procedure that,
; when called on a set of args, returns the negation of applying op. A reminder
; that many logical operators in Racket can take 1 or more arguments (for instance,
; (< 1 2 3 5) is a valid term)

(define (opposite op)
  (lambda (a . x)
  (not (apply op a x))))


; Is this a correct implementation of opposite?
; ANSWER HERE: Yes. All the negations are workng and seems to fail where they should fail

; Procedure 2: skip-n
; This procedures takes a positive integer n and a stream st and returns a stream
; that contains every nth element of st (in stream order), starting with the first.
; The filter must be able to be applied to streams of any length.

(define (skip-n n st)
  (stream-cons (stream-ref st 0)
               (skip-n n (stream-tail st n))))

; Is this a correct implementation of skip-n?
; ANSWER HERE: Skip-n does not handle the case where the index used in the stream-tail function is larger than the length of the stream and this leads to an error.


; tests for opposite
(define opposite-tests
  (test-suite
   "opposite tests"

   ; I did not know that AND can't be passed to apply.
   ; negation of true AND false true 
   (test-equal? "AND - true false"
                ((opposite (lambda (a b) (and a b))) #f #t)
                #t)

   ; negation of false AND false is true 
   (test-equal? "AND - false false"
                ((opposite (lambda (a b) (and a b))) #f #f)
                #t)

   ; negation of true AND true is false 
   (test-equal? "AND - true false"
                ((opposite (lambda (a b) (and a b))) #t #t)
                #f)

   ; negation of true or false is false 
   (test-equal? "OR - true false"
                ((opposite (lambda (a b) (or a b))) #f #t)
                #f)

   ; negation of true or true is false 
   (test-equal? "OR - true false"
                ((opposite (lambda (a b) (or a b))) #t #t)
                #f)

   ; negation of false or false is true 
   (test-equal? "OR - true false"
                ((opposite (lambda (a b) (or a b))) #f #f)
                #t)

   ; negation of equality of equal items is false 
   (test-equal? "Equality - is equal"
                ((opposite equal?) 1 1)
                #f)

   ; negation of equality of unequal items is true
   (test-equal? "Equality - is not equal"
                ((opposite equal?) 1 2)
                #t)
   
   (test-equal? "Non-Equality operation - condition is met"  
                ((opposite >) 2 1)
                #f)
   
   (test-equal? "Non-Equality operation - condition is not met"
                ((opposite >) 2 2)
                #t)

   (test-exn "FAILS - A procedure is not passed"
             exn:fail?
             (λ () ((opposite 42) #t #f)))

   (test-exn "FAILS - and has only 1 argument"
             exn:fail?
             (λ () ((opposite (lambda (a b) (and a b))) #t)))

   (test-exn "FAILS - or has only 1 argument"
             exn:fail?
             (λ () ((opposite (lambda (a b) (or a b))) #t)))

   (test-exn "FAILS - incorrect input"
             exn:fail?
             (λ () ((opposite >) #f 5)))

   (test-equal? "Negation of Negation - True"
                ((opposite (opposite equal?)) 1 1)
                 #t)

   (test-equal? "Negation of Negation - False"
                ((opposite (opposite equal?)) 1 2)
                 #f)

   (test-equal? "Less than or equal - greater than"
                ((opposite <=) 3 2)
                #t)

   (test-equal? "Less than or equal - less than"
                ((opposite <=) 2 3)
                #f)


   (test-equal? "Greater than or equal - less than"
                ((opposite >=) 2 3)
                #t)

   (test-equal? "Greater than or equal - greater than"
                ((opposite >=) 3 2)
                #f)

   (test-equal? "Nested Negations - True"
                ((opposite (opposite equal?)) 1 1)
                 #t)
   (test-equal? "Even Nested Negations - False"
                ((opposite (opposite (opposite equal?))) 1 2)
                 #t)

   (test-equal? "Nested Opposites with AND - True"
                ((opposite (opposite (lambda (a b) (and a b)))) #t #t)
                 #t)

   (test-equal? "Nested Opposites with OR - False"
                ((opposite (opposite (lambda (a b) (or a b)))) #f #f)
                 #f)
   ))

(define skip-n-tests
  (test-suite
   "skip-n tests"
   
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; all tests are written below

; 1. location-of
(define location-of-tests
  (test-suite
   "location-of tests"

   (test-false "element is not in an empty list"
               (location-of '() 'x))
   
   (test-false "element not in the list"
               (location-of '(b c d e f) 'a))
   
   (test-equal? "element at the beginning of the list"
                (location-of '(a b d e f) 'a) 0)

   (test-equal? "element in the middle of the list"
                (location-of '(b c d a e f) 'a) 3)

   (test-equal? "element in the middle of the list"
                (location-of '(b c d e f a) 'a) 5)

   (test-equal? "if element appears multiple times, return the first occurrence"
                (location-of '(b c a d e a f ) 'a) 2)
   ))


; 2. loc
(define loc-tests
  (test-suite
   "loc tests"

   (test-equal? "element is not in an empty list"
               (loc 'x '()) -1)
   
   (test-equal? "element not in the list"
               (loc 'a '(b c d e f)) -1)
   
   (test-equal? "element at the beginning of the list"
                (loc 'a '(a b d e f)) 0)

   (test-equal? "element in the middle of the list"
                (loc 'a '(b c d a e f)) 3)

   (test-equal? "element in the middle of the list"
                (loc 'a '(b c d e f a)) 5)

   (test-equal? "if element appears multiple times, return the first occurrence"
                (loc 'a '(b c a d e a f )) 2)

   (test-equal? "given test 1"
               (loc 3 '(2 4 3 1 2 3)) 2)

   (test-equal? "given test 2"
                (loc 3 '(2 4 3 1 2 3 6)) 2)

   (test-equal? "given test 3"
               (loc 3 '(2 4 6 4 2)) -1)
   ))


; 3. is-in?
(define is-in?-tests
  (test-suite
   "is-in? tests"

   (test-false "element is not in an empty list"
               (is-in? 'a '()))
  
   (test-true "element is a single-element list"
              (is-in? 3 '(3)))
  
   (test-false " element is not in a single-element list"
              (is-in? 3 '(2)))
   
   (test-true "element is single-element nested list"
              (is-in? 3 '((3))))
   
   (test-false "element is not in single-element nested list"
              (is-in? 3 '((2))))

   (test-true "element is in a flat list"
               (is-in? 'a '(b c d a e)))

   (test-false "element is not in a flat list"
               (is-in? 'a '(b c d e)))

   (test-false "element is not in a nested list"
               (is-in? 'x '(a (b c (d)) e f)))

   (test-true "element is in a nested list"
              (is-in? 'x '(a (b x (d y)) e f)))

   (test-true "element is in the rest of the list after sublist"
              (is-in? 'x '(a (b c (d)) e f x)))

   (test-true "element is in any sublist after first sublst"
               (is-in? 'x '(a (b c (d)) (f x) e)))

   (test-true "element is in the last sublist"
              (is-in? 'x '(a (b c (d)) e f (g (h i x)))))))

; 4a. convert-all
(define convert-all-tests
  (test-suite
   "convert-all tests"
   
   (test-equal? "convert-all with empty list"
               (convert-all "mi" '())
               '())
   
   (test-equal? "convert-all a mix of both to miles"
               (convert-all "mi" races)
               '(6.2 1 100 3.1))

   (test-equal? "convert-all a mix of both to kilometers"
               (convert-all "km" races)
               '(10 1.61 161.0 5))

   (test-equal? "convert-all km only to mi"
               (convert-all "mi" '(("Boston 10K" 10 "km")
                                     ("Portland Mile" 1 "km")
                                     ("Cleveland 100" 100 "km")
                                     ("Oberlin 5K" 5 "km")))
               '(6.2 0.62 62.0 3.1))

   (test-equal? "convert-all km only to km"
               (convert-all "km" '(("Boston 10K" 10 "km")
                                     ("Portland Mile" 1 "km")
                                     ("Cleveland 100" 100 "km")
                                     ("Oberlin 5K" 5 "km")))
               '(10 1 100 5))

   (test-equal? "convert-all mi only to km"
               (convert-all "km" '(("Boston 10K" 10 "mi")
                                     ("Portland Mile" 1 "mi")
                                     ("Cleveland 100" 100 "mi")
                                     ("Oberlin 5K" 5 "mi")))
               '(16.1 1.61 161.0 8.05))

   (test-equal? "convert-all mi only to mi"
               (convert-all "mi" '(("Boston 10K" 10 "mi")
                                     ("Portland Mile" 1 "mi")
                                     ("Cleveland 100" 100 "mi")
                                     ("Oberlin 5K" 5 "mi")))
               '(10 1 100 5))
   
   (test-exn "invalid race format gives error"
             exn:fail?
             (λ () (convert-all "mi" '(("Boston 10K" 10)
                               ("Portland Mile" 1 "mi")))))
   ))


; 4b. c-a
(define c-a-tests
  (test-suite
   "c-a tests"
   
   (test-equal? "c-a with empty list"
               (c-a "mi" '())
               '())
   
   (test-equal? "c-a a mix of both to miles"
               (c-a "mi" races)
               '(6.2 1 100 3.1))

   (test-equal? "c-a a mix of both to kilometers"
               (c-a "km" races)
               '(10 1.61 161.0 5))

   (test-equal? "c-a km only to mi"
               (c-a "mi" '(("Boston 10K" 10 "km")
                                     ("Portland Mile" 1 "km")
                                     ("Cleveland 100" 100 "km")
                                     ("Oberlin 5K" 5 "km")))
               '(6.2 0.62 62.0 3.1))

   (test-equal? "c-a km only to km"
               (c-a "km" '(("Boston 10K" 10 "km")
                                     ("Portland Mile" 1 "km")
                                     ("Cleveland 100" 100 "km")
                                     ("Oberlin 5K" 5 "km")))
               '(10 1 100 5))

   (test-equal? "c-a mi only to km"
               (c-a "km" '(("Boston 10K" 10 "mi")
                                     ("Portland Mile" 1 "mi")
                                     ("Cleveland 100" 100 "mi")
                                     ("Oberlin 5K" 5 "mi")))
               '(16.1 1.61 161.0 8.05))

   (test-equal? "c-a mi only to mi"
               (c-a "mi" '(("Boston 10K" 10 "mi")
                                     ("Portland Mile" 1 "mi")
                                     ("Cleveland 100" 100 "mi")
                                     ("Oberlin 5K" 5 "mi")))
               '(10 1 100 5))
   
   (test-exn "invalid race format gives error"
             exn:fail?
             (λ () (c-a "mi" '(("Boston 10K" 10)
                               ("Portland Mile" 1 "mi")))))
   ))

(define all-tests
  (test-suite
   "all-tests"
   location-of-tests
   loc-tests
   is-in?-tests
   convert-all-tests
   c-a-tests
   opposite-tests
   skip-n-tests
   ))

(run-tests all-tests)



