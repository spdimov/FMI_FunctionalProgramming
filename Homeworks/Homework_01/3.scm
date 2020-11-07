#! zad3

(define (remove-multiple-whitespaces str) ; премахва само празните пространства, по-големи от " "
  (define (loop i len new-str)
    (if (= i (- len 1))
        (string-append new-str (string (string-ref str (- len 1))))
        (if (and (string=? (string (string-ref str i)) " ") (string=? (string (string-ref str (+ i 1))) " "))
            (loop (+ i 1) len new-str)
            (loop (+ i 1) len (string-append new-str (string (string-ref str i))))
            )))
  (if (string=? str "")
      " "
      (loop 0 (string-length str) "")))

(define (remove-all-whitespaces str)
  (define (loop i new-str)
    (if (= i (string-length str))
        new-str
        (if (string=? (string (string-ref str i)) " ")
            (loop (+ i 1) new-str)
            (loop (+ i 1) (string-append new-str (string (string-ref str i)))))))
  (loop 0 ""))

(define (first-space expr) ; изразът не трябва да започва с празно пространсто
  (if (string=? (string (string-ref expr 0)) " ")
      (string-tail expr 1)
      expr))

(define (last-space expr) ; изразът трябва да завършва с празно пространство,заради проверките за следващи символи
  (if (string=? (string (string-ref expr (- (string-length expr) 1))) " ")
      (string-append expr " ")
      (string-append expr "  ")))

(define (adjust-expr expr)
  (if (string=? expr "")
      expr
      (first-space (last-space expr)))
)

(define (digit? str)
  (string->number str))

(define (arithmetic? expr)
  (if (or (string=? (string (string-ref expr 0)) "+")
          (string=? (string (string-ref expr 0)) "-")
          (string=? (string (string-ref expr 0)) "*")
          (string=? (string (string-ref expr 0)) "/")
          (string=? (string (string-ref expr 0)) "^"))
      #t
      #f))

(define (string-tail str n)
  (define (loop i newstr)
    (if (= i (string-length str))
        newstr
        (if (> i (- n 1))
            (loop (+ i 1) (string-append newstr (string (string-ref str i))))
            (loop (+ i 1) newstr))))
  (loop 0 ""))

(define (string-head str n)
  (define (loop i newstr)
    (if (= i (string-length str))
        newstr
        (if (< i n)
            (loop (+ i 1) (string-append newstr (string (string-ref str i))))
            (loop (+ i 1) newstr))))
  (loop 0 ""))

(define (get-number expr)
  (define (loop number expr)
        (if (or (string=? expr "") (eq? (string->number (string (string-ref expr 0))) #f))
            number
            (loop (string-append number (string (string-ref expr 0))) (string-tail expr 1))))
            
  (loop "" expr))



(define (expr-valid? expr_org)
  (define (loop i expr)
    (if (string=? expr " ") 
        #t
        (if (= (remainder i 2) 0) ;всеки елемент на четна позиция (вкл. на нулева) трябва да бъде число, а всеки елемент на нечетна позиция - оператор
            (if (string->number (get-number expr)) ; проверка дали настоящия елемент е число
                (if (string=? (string (string-ref expr (string-length (get-number expr)))) " ") ; проверяваме дали има празно пространство между елементите, за да знаем, откъде ще започне следващия елемент
                    (loop (+ i 1) (string-tail expr (+ 1 (string-length (get-number expr)))))
                    (loop (+ i 1) (string-tail expr (string-length (get-number expr)))))
                #f)
            (if (and (arithmetic? (string-head expr 1)) (or (string->number (string (string-ref expr 2))) (string->number (string (string-ref expr 1))))) ; ако имаме оператор, трябва следващия символ да бъде число, ако изразът е валиден
                (if (string=? (string (string-ref expr 1)) " ") ; аналогично, както при числата, проверяваме за празно пространство след оператора
                    (loop (+ i 1)  (string-tail expr 2))
                    (loop (+ i 1)  (string-tail expr 1)))
                #f)
            )
        )
    )
  (loop 0 (adjust-expr (remove-multiple-whitespaces expr_org)))
  )
            

(define (string-reverse str)
  (define (loop i rev)
    (if (= i (string-length str))
        rev
        (loop (+ i 1) (string-append (string (string-ref str i)) rev))))
  (loop 0 ""))

(define (operator->precedence op)
  (cond ((string=? op "+") 1)
        ((string=? op "-") 1)
        ((string=? op "*") 2)
        ((string=? op "/") 2)
        ((string=? op "^") 3)
        ))
        
(define (higher-precedence? op1 op2)
  (if (> (operator->precedence op1) (operator->precedence op2))
      #t
      #f))

(define (stack-top stack) ;работи за оператори само
  (string-tail stack (- (string-length stack) 1))
  )

(define (stack-pop stack) ;работи за оператори само
 (string-head stack (- (string-length stack) 1))
  )

(define (current-operator expr)
  (string (string-ref expr 0))
  )
 
(define (expr-rp expr)
  (define (loop new stack expr)
    (if (string=? expr "")
       (string-tail (string-append new (string-reverse stack)) 1)
        (if (string->number (get-number expr))
            (loop (string-append new "," (get-number expr)) stack (string-tail expr (string-length (get-number expr)))) 
            (if (string=? stack "")
                (loop new (string-append stack (current-operator expr)) (string-tail expr 1))
                (if (higher-precedence? (string (string-ref expr 0)) (stack-top stack))
                    (loop new (string-append stack (current-operator expr)) (string-tail expr 1))
                    (loop (string-append new (stack-top stack)) (stack-pop stack)  expr)
                    )
                )
            )
        )
    )
  (if (expr-valid? expr)
      (loop "" "" (remove-all-whitespaces expr))
      #f)
  )

(define (stack-push-numbers stack number)
  (string-append stack "," number ))

(define (stack-top-numbers stack)
 (string-reverse (get-number (string-reverse stack))))

(define (stack-pop-numbers stack)
  (string-head stack (- (- (string-length stack) (string-length (stack-top-numbers stack))) 1)))

(define (string->operator str)
  (cond ((string=? str "+") (lambda (x y) (+ x y)))
        ((string=? str "-") (lambda (x y) (- y x)))
        ((string=? str "*") (lambda (x y) (* x y)))
        ((string=? str "/") (lambda (x y) (/ x y)))
        ((string=? str "^") (lambda (x y) (expt y x)))))

(define (apply-on stack op)
       (op (string->number (stack-top-numbers stack)) (string->number (stack-top-numbers (stack-pop-numbers stack))))
   )

(define (expr-eval expr)
  (define (loop stack expr)
    (if (string=? expr "")
       (if (string->number (string-tail stack 1))
           (string->number (string-tail stack 1))
           0)
        (cond ((string->number (get-number expr)) (loop (stack-push-numbers stack (get-number expr)) (string-tail expr (string-length (get-number expr)))))
              ((string=? (string (string-ref expr 0)) ",") (loop stack (string-tail expr 1)))
              (#t (loop (string-append (stack-pop-numbers (stack-pop-numbers stack)) "," (number->string (apply-on stack (string->operator (string (string-ref expr 0)))))) (string-tail expr 1)))
              )
        ))
  (loop "" (expr-rp expr)))