#lang racket

(provide (all-defined-out))

(define (remove-whitespace str)
  (define (loop i result)
    (cond
      ((>= i (string-length str)) result)    
      ((char-whitespace? (string-ref str i)) (loop (+ i 1) result))
      (else (loop (+ i 1) (string-append result (string (string-ref str i)))))
    )
  )
  (loop 0 "")
)


(define (tree? str)
  (define node (remove-whitespace str))
  (cond ((string=? "*" node) #t)
        ((string=? "" node) #f)
        ((check-parentheses node) (and (tree? (get-left (remove-parentheses node))) (tree? (get-right (remove-parentheses node)))))
        (else #f)
      )
  )

(define (check-parentheses node) ;;every tree should start with '{' and end with '}'
  (if (and (eq? (string-ref node 0) #\{)
           (eq? (string-ref node (- (string-length node) 1)) #\}))
      #t
      #f))

(define (get-left node)
  (define (helper pos)
    (if (>= pos (string-length node)) ""
    (if (char-numeric? (string-ref node pos))
        (helper (+ pos 1))
        (substring node pos (find-right-node node))
        )
    )
    )
  (helper 0)
  )

(define (get-right node)
  (substring node (find-right-node node) (string-length node))
  )
        
  
(define (find-right-node str) ;;return start index of right node
  (define (helper cnt pos flag)
    (cond
      ((= pos (string-length str)) pos)
      ((and (eqv? (string-ref str pos) #\}) (= cnt 1)) (+ 1 pos))
      ((and (equal? (string-ref str pos) #\*) (= cnt 0))  (+ 1 pos))
      ((eqv? (string-ref str pos) #\{) (helper (+ cnt 1) (+ pos 1) #t))
      ((eqv? (string-ref str pos) #\}) (helper (- cnt 1) (+ pos 1) #t))
      (else (helper cnt (+ pos 1) #f))
      )
    )
  (helper 0 0 #f))

(define (remove-parentheses node)
  (if (not (string=? node "*"))
      (substring node 1 (- (string-length node) 1))
      node))

(define (get-number node)
  (define (helper pos)
    (if (char-numeric? (string-ref node pos))
        (helper (+ pos 1))
        (string->number (substring node 0 pos))
        )
    )
  (helper 1))

(define (string->tree str)
  (define node (remove-whitespace str))
  
  (if (tree? str)
  
      (cond ((string=? node "*") '())
            (else       (list (get-number (remove-parentheses node))
                        (string->tree (get-left (remove-parentheses node)))
                        (string->tree (get-right (remove-parentheses node)))))
      )
      #f
   )
  )

(define (left-node node) (cadr node))
(define (right-node node) (caddr node))
(define (node->value node)
  (cond ((null? node) 0)
        (else (car node))
        )
  )
(define (get-left-value node) (node->value (left-node node)))
(define (get-right-value node) (node->value (right-node node)))

(define (height node)
  (cond ((null? node) 0)
        (else (+ 1 (max (height (left-node node)) (height (right-node node)))))
        )
  )

(define (balanced? tree)
  (cond ((null? tree) #t)
        ((and (<= (abs (- (height (left-node tree)) (height (right-node tree)))) 1)
              (balanced? (left-node tree)) (balanced? (right-node tree))) #t)
        (else #f))
  )


(define (max-element tree)
    (cond ((null? tree) 0)
          (else (max (car tree) (max-element (left-node tree)) (max-element (right-node tree))))
          )
  )
  
(define (ordered? tree) ;;equal numbers are placed in the left tree
  
  (define (helper node min max)
    (cond
      ((null? node) #t)
      ((or (< (node->value node) min) (> (node->value node) max)) #f)
      ((null? (cadr node)) #t)
      (else (and (helper (left-node node) min (node->value node))
                 (helper (right-node node) (+ (node->value node) 1) max)))    
      )
    )
  (helper tree 0 (max-element tree))
  )


(define (close-parentheses n)
  (define (loop i result)
    (cond ((= i n) result)
          (else (loop (+ i 1) (string-append result "}")))
          )
    )
  (loop 0 ""))

(define (tree->string tree) ;;flag is true when we have to close parentheses in right node
  (define (helper tree deep flag)
    (cond ((and (null? tree) flag) (string-append " *" (close-parentheses deep)))
          ((null? tree) " *")
          (else (string-append " {" (number->string (node->value tree))    
                               (helper (left-node tree) 0 #f)
                               (helper (right-node tree) (+ deep 1) #t)
                               )
                )
          )
    )
  (define result (helper tree 0 #f))
  (substring result 1 (string-length result)) ;;removing whitespace at first position
  )


(define (preorder-stream tree)
  (cond ((null? tree) empty-stream)
        (else (stream-append  (stream (node->value tree))
                              (preorder-stream (left-node tree))
                              (preorder-stream (right-node tree))    
                       )
              )
        )
  )

(define (inorder-stream tree)
  (cond ((null? tree) empty-stream)
        (else (stream-append   (inorder-stream (left-node tree))
                               (stream (node->value tree))
                               (inorder-stream (right-node tree))    
                       )
              )
        )
  )

(define (postorder-stream tree)
  (cond ((null? tree) empty-stream)
        (else (stream-append   (postorder-stream (left-node tree))
                               (postorder-stream (right-node tree))
                               (stream (node->value tree))
                       )
              )
        )
  )

(define (tree->stream tree order)
  (case order
    ((preorder) (preorder-stream tree))
    ((inorder) (inorder-stream tree))
    ((postorder) (postorder-stream tree))
    )
  )


(define test-tree '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))


  
  (define (preorder tree level)
    (cond ((null? tree) '())
          (else (append (list (list (node->value tree) level))
                        (preorder (right-node tree) level)
                        (preorder (left-node tree) (+ 1 level))
                        )
                )
          )
    )
  
  
  
  (define groups '(0 1 2))

  (define (get-results-by-group lst group)
    (define (helper lst result)
      (cond ((null? lst) (reverse result))
            ((equal? (cadar lst) group) (helper (cdr lst) (cons (caar lst) result)))
            (else (helper (cdr lst) result))
            )
      )
    (helper lst '()))

  (define (group-by lst)
    (map (lambda (x) (list x (get-results-by-group lst x)))
         groups))
  
         
        
  