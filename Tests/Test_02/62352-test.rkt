;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Станислав Димов
; ФН: 62352
; Специалност: СИ
; Курс: 3
; Административна група: 5
; Начален час на контролното: <тук попълнете часа за вашата група>
;

#lang racket/base

(require "62352.rkt")
(require rackunit rackunit/gui)

(test/gui

 (test-suite
  "weight-balanced? returns #t for balanced trees"
  (test-true "empty tree is balanced" (weight-balanced? '()))
  (test-true "(1 () ())" (weight-balanced? '(1 () ())))
  (test-true "(1 (2 () ()) (3 () (4 () ())))" (weight-balanced? '(1 (2 () ()) (3 () (4 () ())))))
  (test-true "(1 () (2 () ()))" (weight-balanced? '(1 () (2 () ()))))
  (test-true "(1 (2 () ()) (3 () ()))" (weight-balanced? '(1 (2 () ()) (3 () ()))))
  )

 (test-suite
  "weight-balanced? returns #f for not balanced trees"
  (test-false "(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))" (weight-balanced? '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
  (test-false "(1 (2 () (3 () ())) ())" (weight-balanced? '(1 (2 () (3 () ())) ())))
  (test-false "(1 (2 (3 () ()) ()) ())" (weight-balanced? '(1 (2 (3 () ()) ()) ())))
  (test-false "(1 () (2 (3 () ()) ()))" (weight-balanced? '(1 () (2 (3 () ()) ()))))
  (test-false "(1 () (2 () (3 () ())))" (weight-balanced? '(1 () (2 () (3 () ())))))
  )

 (test-suite
  "attempts return correct result"
  (test-equal? "((1 a 2) (2 a 4))" (attempts 'a '((1 a 2) (2 a 4))) '((1 1) (2 1)))
  (test-equal? "((1 a 2) (2 a 4) (1 a 3) (3 b 3))" (attempts 'a '((1 a 2) (2 a 4) (1 a 3) (3 b 3))) '((1 2) (2 1) (3 0)))
 )
)