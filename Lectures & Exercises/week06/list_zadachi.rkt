;;zad1 Да се напишат функциите (take n lst) и (drop n lst), който съответно взимат или премахват първите n елемента на списък

(define (take n lst)
  (cond ((null? lst) lst)
        ((= n 0) '())
        (else (cons (car lst) (take (- n 1) (cdr lst))))
        )
  )

(define (drop n lst)
  (cond ((null? lst) lst)
        ((= n 0) lst)
        (else (drop (- n 1) (cdr lst)))
        )
  )



;;zad2 а се напишат функциите (all? p? lst) и (any? p? lst), които проверяват съответно дали всички или някои елементи на даден списък изпълняват предиката p?

(define (all? p? lst)
  (cond ((null? lst) #t)
        ((not (p? (car lst))) #f)
        (else (all? p? (cdr lst)))
        )
  )

(define (any? p? lst)
  (cond ((null? lst) #f)
        ((p? (car lst)) #t)
        (else (any? p? (cdr lst)))
        )
  )

;;zad3 Да се напише функция (zip lst1 lst2), която приема два списъка и връща списък от наредени двойки от техните съответни елементи


(define (zip lst1 lst2)
    (cond ((or (null? lst1) (null? lst2)) '())
          (else (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))))
          )
  )

;;zad5 Да се напише функция (sorted? lst), която проверява дали списък е сортиран в ненамаляващ ред

(define (sorted? lst)
  (define (helper last lst)
    (cond ((null? lst) #t)
          ((> last (car lst)) #f)
          (else (helper (car lst) (cdr lst)))
          )
    )
  (helper (car lst) lst)
  )

;;zad6 Да се напише функция (uniques lst), която оставя само уникалните стойности в даден списък. Можете да проверявате за еднаквост с equal? за най-сигурно

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
        (else (filter p? (cdr lst)))
        )
  )

  