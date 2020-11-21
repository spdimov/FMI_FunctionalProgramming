#lang racket/base

(require rackunit rackunit/gui racket/include)

(include "list_zadachi.rkt")

  

(test/gui
 (test-suite
  "test take procedure"
 (test-equal? "List (1 2 3 4 5) should become (1 2)" (take  2 '(1 2 3 4 5)) '(1 2))
 (test-equal? "List (1 2 3 4 5) should become (1 2 3)" (take  3 '(1 2 3 4 5)) '(1 2 3))
 (test-equal? "List (1 2 3 4 5) should become ()" (take  0 '(1 2 3 4 5)) '())
 (test-equal? "List () should become ()" (take  5 '()) '())
    )

 (test-suite
  "test drop procedure"
  (test-equal?  "List (1 2 3 4 5) should become (3 4 5)" (drop  2 '(1 2 3 4 5)) '(3 4 5))
  (test-equal?  "List (1 2 3 4 5) should become (5)" (drop  4 '(1 2 3 4 5)) '(5))
  (test-equal?  "List (1 2 3 4 5) should become ()" (drop  6 '(1 2 3 4 5)) '())
  (test-equal?  "List () should become ()" (drop  2 '()) '())
 )

 (test-suite
  "test all? predicate"
  (test-true "All numbers are even in (2 4 6 8)" (all? even? '(2 4 6 8)))
  (test-true "All numbers are odd in (1 3 5 7)" (all? odd? '(1 3 5 7)))
  (test-false "Not all numbers are even in (2 3 6 8)" (all? even? '(2 3 6 8)))
  (test-false "Not numbers are odd in (2 3 6 8)" (all? even? '(2 3 6 8)))
 )

 (test-suite
  "test zip procedure"
  (test-equal? "zip (1 2) (3 4)" (zip '(1 2) '(3 4)) '((1 . 3) (2 . 4)))
  (test-equal? "zip (1 2) (3 4 5)" (zip '(1 2) '(3 4 5)) '((1 . 3) (2 . 4)))
  (test-equal? "zip (1) (3 4)" (zip '(1) '(3 4)) '((1 . 3)))
 )
)1