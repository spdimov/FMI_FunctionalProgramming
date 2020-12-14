#lang racket/base

(define (print lst)
  (if (null? lst)
      (display "End")
      (begin
        (display (car lst))
        (display " ")
        (print (cdr lst))
        )
      ))

(define (length-rec lst) ;;recursive
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))
  )
)

(define (length-iter lst) ;;iterative
  (define (loop lst result)
    (if (null? lst) result
        (loop (cdr lst) (+ 1 result))
        )
    )
  (loop lst 0)
  )

(define (interval a b)
  (if (> a b) '()
      (cons a
            (interval (+ a 1) b)))
  )
        
(define (collect-even a b)
  (cond ((> a b) '())
        ((even? a) (cons a
                         (collect-even (+ a 1) b)))
        (else (collect-even (+ a 1) b))
        )
  )

(define (append lst1 lst2) ;;recursive
  (cond ((null? lst1) lst2)
        (else (cons (car lst1)
                    (append (cdr lst1) lst2)))
        )
  )

(define (append-iter lst1 lst2) ;;iterative
  (define (loop L result)
    (if (null? L) result
        (loop (cdr L) (cons (car L) result))
        ))
  (loop (reverse lst1) lst2)
  )

(define (reverse L)
  (define (loop L result)
    (if (null? L) result
        (loop (cdr L) (cons (car L) result)) ))
  (loop L '())
  )

(define (flatten L)
  (cond ((null? L) '())

        ((pair? (car L))
        (append (flatten (car L))
                (flatten (cdr L)) ))
        (else (cons (car L) (flatten (cdr L)) ))
        ))

(define (map op L)
  (if (null? L) '()
      (cons (op (car L)) (map op (cdr L))
            )
      )
  )

(define (filter p? L)
  (cond ((null? L) L)
        ((p? (car L)) (cons (car L) (filter p? (cdr L))))
        (else (filter p? (cdr L)))
        )
  )

(define (foldr op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (foldr op init (cdr lst)))
      )
  )

(define (foldl op init lst)
  (define (loop left result)
    (if (null? left) result
        (loop (cdr left)
              (op (car left) result)))
    )
  (loop lst init))
        
                
      