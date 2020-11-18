(define (len list) ;;намира дължината на списък
  (cond ((null? list) 0)
        (else (+ 1 (len (cdr list))))
        )
  )

(define (exists? l p) ;;проверява дали съществува елемент в l, за който е изпълнен предикатът p.
  (cond ((null? l) #f)
        ((p (car l)) #t)
        (else (exists? (cdr l) p))
        )
  )

(define (member? l x) ;;която проверява дали елементът x се съдържа в списъка l
  (cond ((null? l) #f)
        ((= (car l) x) #t)
        (else (member? (cdr l) x))
        )
  )

(define (at n l) ;;която връща елементът, намиращ се на позиция n (броим от 0) в списъка l или #f, ако позицията е извън списъка
  (define (loop pos l)
    (cond ((null? l) #f)
          ((= pos n) (car l))
          (else (loop (+ pos 1) (cdr l)))
          )
    )
  (loop 0 l))

(define (map_custom f l) ;;която прилага f върху всеки елемент на списъка l
  (cond ((null? l) '())
        (else (cons (f (car l)) (map_custom f (cdr l)))))
  )

(define (filter p l) ;; която съставя нов списък, съдържащ само елементите на l, за които е изпълнен предикатът p
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else        (filter p (cdr l)))
        )
  )

(define (push x l) ;;която добавя елемента x на края на списъка l
  (cond ((null? l) (cons x '()))
        (else (cons (car l) (push x (cdr l))))
        )
  )

(define (reverse-list l) ;;която връща списък с елементите на l в обратен ред
  (cond ((null? l) l)
        (else (append (reverse (cdr l)) (list (car l))))
        )
  )
  
(define (insert x n l) ;; която вкарва елемента x на позиция n в списъка l (ако n е след края на l, вкарваме x накрая)
  (cond ((null? l) (list x))
        ((zero? n) (cons x l))
        (else (cons (car l) (insert x (- n 1) (cdr l))))
        )
  )

(define (accumulate l op init) ;;която пресмята (op l[0] (op l[1] (op l[2] ... (op l[n] init) ... ))) (ако имаме подаден празен списък, резултатът е init).

  (define (loop l)
    (cond ((null? l) init)
          (else (op (car l) (loop (cdr l))))
          )
    )
  (loop l)
  )

(define (sum l) ;;която намира сумата на елементите в списък
  (accumulate l + 0))

