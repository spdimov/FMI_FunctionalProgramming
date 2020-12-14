#lang racket/base

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (tree? (cadr t))
           (tree? (caddr t))
           )
      )
  )

(define (invert tree)
  (if (null? tree)
      tree
      (make-tree (root-tree tree)
                 (invert (right-tree tree))
                 (invert (left-tree tree))
                 )
      )
  )

(define (bst-insert val t)
  (cond ((null? t) (make-leaf val))
        ((< val (root-tree t))
         (make-tree (root-tree t)
                    (bst-insert val (left-tree t))
                    (right-tree t)))
        (else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t))))
        )
  )

(define (height node)
  (cond ((null? node) 0)
        (else (max (+ 1 (height (right-tree node)))
                   (+ 1 (height (left-tree node)))))
        )
  )

;;matrix

(define (at m i j)
  (list-ref (list-ref m i) j))

(define (mat-map f m)
  (map (lambda (x) (map f x)) m))


(define (all-numbers? lst)
  (cond ((null? lst) #t)
        ((number? (car lst)) (all-numbers? (cdr lst)))
        (else #f)))

(define (map? m)
 (define (loop m len)
   (cond
     ((null? m) #t)
     ((not (= (length (car m)) len)) #f)
     ((not (all-numbers? (car m))) #f)
     (else (loop (cdr m) len))
     )
   )
  (loop m (length (car m)))
  )
                    