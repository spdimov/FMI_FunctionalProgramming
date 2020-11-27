;;zad 10 Да се напише функция (compose . fns), която приема произволен брой функции като аргументи и връща тяхната композиция:

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l)
          (foldr op init (cdr l)))
      )
  )

(define (compose . fns)
  (define (id x) x)

  (define (single-compose f1 f2)
    (lambda (x)
      (f1 (f2 x))
      )
    )

  (foldr single-compose id fns)
  )

(define (sq x) (* x x))
(define (1+ x) (+ x 1))
(define f (compose sq 1+ (lambda (x) (* x 2)) 1+))

;;zad11 Да се напише функция (group-by f lst), която групира елементите на списъка lst по стойността, която f връща за тях:

(define (filter p? lst)
  (cond ((null? lst) lst)
        ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
        (else (filter p? (cdr lst)))
        )
  )

(define (uniques lst)
  (cond ((null? lst) lst)
        (else (cons (car lst) (uniques (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst)))))
        )
  )

(define (get-results-list el f lst result)
  (cond ((null? lst) result)
        ((equal? el (f (car lst))) (get-results-list el
                                                     f
                                                     (cdr lst)
                                                     (cons (car lst) result)))
        (else (get-results-list el
                                f
                                (cdr lst)
                                result))
        )
  )

(define (group-by f lst)
  (map (lambda (x)
         (list x (get-results-list x f lst '())))
       (uniques (map f lst))))

