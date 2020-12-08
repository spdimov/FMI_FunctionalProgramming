#lang racket

(require rackunit rackunit/gui racket/include)

(require "tree.rkt")

(define (expect-correct expr)
  (test-true
    (string-append "Tree '" expr "' should be correct")
    (tree? expr)
  )
)

(define (expect-incorrect expr)
  (test-false
    (string-append "Tree '" expr "' should be incorrect")
    (tree? expr)
  )
)

(define (expect-correct-list expr lst)
  (test-equal?
   (string-append "String " expr " should become correct tree")
   (string->tree expr)
   lst)
  )

(define (compare-list-and-stream L S)
  (equal? L (stream->list S))
   )

(test/gui

 (test-suite
  "Tree validation"
  (expect-correct "{5**}")
  (expect-correct "*")
  (expect-correct "{5{3*  *}  {2**}}")
  (expect-correct "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
  (expect-incorrect "{5 {22 {2 * } {6 * *}} {1 * {3 {111 * *} *}}}")
  (expect-incorrect "abc")
  (expect-incorrect "")
  (expect-incorrect "{}")
  (expect-incorrect "{6{}{4**}}")
  (expect-incorrect "{5 {5}")
  (expect-incorrect "{{{")
  (expect-incorrect "{{}")
  )

(test-suite
  "Remove-whitespace works correctly"

  (test-equal?
   "Expressions without whitespace are left intact"
   (remove-whitespace "123abc")
   "123abc")

 (test-equal?
  "Whitespace is removed"
  (remove-whitespace " \t  123 \t abc \t  ")
  "123abc")

  (test-equal?
  "An all-whitespace expression is processed correctly"
  (remove-whitespace "  \t \n \t  ")
  "")
 )
 
 (test-suite
  "Valid strings are evaluated correctly"
  (expect-correct-list "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}" '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) )
  (expect-correct-list "{5**}" '(5 () ()) )
  (expect-correct-list "{10 {3{4 * *}*} {9**}}" '(10 (3 (4 () ()) ()) (9 ()())) )
  (expect-correct-list "{10 {3 * *} {9**}}" '(10 (3 () ()) (9 ()())) )
 )

 (test-suite
  "string->tree returns #f for incorrect strings"
  (test-false "{{" (string->tree "{{"))
  (test-false "{10 {3 * *} {9**}" (string->tree "{10 {3 * *} {9**}"))
  (test-false "{5  {{3 4}}" (string->tree "{5  {{3 4}}"))
 )

 (test-suite
  "balanced? returns #t for balanced trees"
  (test-true "(5 (22 (2 () ()) (6 () ())) (1 () (3 () ())))" (balanced? '(5 (22 (2 () ()) (6 () ())) (1 () (3 () ())))))
  (test-true "(3 (2 () ()) ())" (balanced? '(3 (2 () ()) ())))
  (test-true "(5 () ())" (balanced? '(5 () ())))
  (test-true "()" (balanced? '()))
  )

 (test-suite
  "balanced? returns #f for not balanced trees"
  (test-false "(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))" (balanced? '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
  (test-false "(1 (2 (3 () ()) ()) ())" (balanced? '(1 (2 (3 () ()) ()) ())))
  (test-false "(1 (2 () (3 () ()) ())" (balanced? '(1 (2 () (3 () ())) ())))
  (test-false "'(1 () (2 (3 () ()) ()))" (balanced? '(1 () (2 (3 () ()) ()))))
  )

 (test-suite
  "ordered? returns #t for ordered"
  (test-true "(10 (2 () ()) ())" (ordered? '(10 (2 () ()) ())))
  (test-true "(10 (8 (7 () ()) (9 () ())) (15 () ()))" (ordered? '(10 (8 (7 () ()) (9 () ())) (15 () ()))))
  (test-true "(1 () ())" (ordered? '(1 () ())))
  (test-true "()" (ordered? '()))
  )

 (test-suite
  "ordered? returns #f for not ordered"
  (test-false "(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))" (ordered? '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
  (test-false "(10 (7 (8 () ()) (9 () ())) (15 () ()))" (ordered? '(10 (7 (8 () ()) (9 () ())) (15 () ()))))
  (test-false "(10 (7 (11 () ()) (9 () ())) (15 () ()))" (ordered? '(10 (7 (11 () ()) (9 () ())) (15 () ()))))
  (test-false "(10 (6 (7 (11 () ()) ()) ()) ())" (ordered? '(10 (6 (7 (11 () ()) ()) ()) ()))) 
  )

 (test-suite
  "tree->string converts tree to string correctly"
  (test-equal? "(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))"
               (tree->string '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
               "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
  
  (test-equal? "(2 (4 () ()) ())"
               (tree->string '(2 (4 () ()) ()))
               "{2 {4 * *} *}")
  (test-equal? "(5 (3 () ()) (2 () ()))"
               (tree->string '(5 (3 () ()) (2 () ())))
               "{5 {3 * *} {2 * *}}")

  )

 (test-suite
  "tree->stream convert tree to stream inorder"
  (test-true "(1 (2 () ()) (3 () ()))" (compare-list-and-stream '(2 1 3) (tree->stream '(1 (2 () ()) (3 () ())) 'inorder)))
  (test-true "(1 (2 (4 () ()) (5 () ())) (3 () ()))" (compare-list-and-stream '(4 2 5 1 3) (tree->stream '(1 (2 (4 () ()) (5 () ())) (3 () ())) 'inorder)))
  (test-true "()" (compare-list-and-stream '() (tree->stream '() 'inorder)))
 )

 (test-suite
  "tree->streams convert tree to stream preoder"
  (test-true "(1 (2 () ()) (3 () ()))" (compare-list-and-stream '(1 2 3) (tree->stream '(1 (2 () ()) (3 () ())) 'preorder)))
  (test-true "(1 (2 (4 () ()) (5 () ())) (3 () ()))" (compare-list-and-stream '(1 2 4 5 3) (tree->stream '(1 (2 (4 () ()) (5 () ())) (3 () ())) 'preorder)))
  (test-true "()" (compare-list-and-stream '() (tree->stream '() 'preorder)))
 )

 (test-suite
  "tree->streams convert tree to stream postorder"
  (test-true "(1 (2 () ()) (3 () ()))" (compare-list-and-stream '(2 3 1) (tree->stream '(1 (2 () ()) (3 () ())) 'postorder)))
  (test-true "(1 (2 (4 () ()) (5 () ())) (3 () ()))" (compare-list-and-stream '(4 5 2 3 1) (tree->stream '(1 (2 (4 () ()) (5 () ())) (3 () ())) 'postorder)))
  (test-true "()" (compare-list-and-stream '() (tree->stream '() 'postorder)))
 )
 )


        
