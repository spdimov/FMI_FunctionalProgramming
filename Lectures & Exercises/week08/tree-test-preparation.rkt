#lang racket/base

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (invert tree)
  (if (null? tree)
      tree
      (make-tree (root-tree tree)
                 (invert (right-tree tree))
                 (invert (left-tree tree))
                 )
      )
  )
                    