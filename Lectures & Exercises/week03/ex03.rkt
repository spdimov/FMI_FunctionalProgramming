#! zad1

(define (constantly c)
  (lambda (x) c))

#! zad2

(define (flip f)
  (lambda (x y) (f y x)))

#! zad3

(define (complement p)
  (lambda (x)
    (if (p x) #f #t)))

#! zad4

(define (compose f g)
  (lambda (x) (f (g x))))

#! zad5

(define (repeat f N)
  (cond ((= N 0) (lambda (x) x))
        ((= N 1) f)
        (#t (compose f (repeat f (- N 1))))))

#! zad6

(define dx 0.000001)
(define (derive  f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

#! zad7

(define (repeated f N)
  (cond ((= N 0) (lambda (x) x))
        ((= N 1) f)
        (#t (lambda (x) (f ((repeated f (- N 1)) x))))))

(define (derive-n n f)
  (repeated derive n))
  


#! zad8

(define (accumulate init op a next b)
  (define (loop i)
    (if (> i b)
        (op (loop (next i)) i)
        init
        ))
  (loop a))

(define (!! n)
  (accumulate 1 (lambda (x y) (* x y)) n (lambda (x) (- x 2)) 1))

#! zad9

#! zad10
(define (accumulate-i init op a next b)
  (define (loop result i)
    (if (<= i b)
        (loop (op result i) (next i))
        result))
  (loop init a))

(define (2^ n)
  (accumulate-i 1 (lambda (x y) (* x 2)) 1 (lambda (x) (+ x 1)) n))
  
#! zad 11

(define (filter-accumulate-11 p? op init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? b i) (op i (loop (next i))) )
          (else (loop (next i)))
          ))
  (loop a))
  
(define (devisor? number a)
  (if (= (remainder number a) 0)
      #t
      #f))

(define (devisors-sum n)
  (filter-accumulate devisor? + 0 1 (lambda (x) (+ x 1)) n))

#! zad 12
(define (filter-accumulate p? op init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? i) (op 1 (loop (next i))) )
          (else (loop (next i)))
          ))
  (loop a))

(define (count p? a b)
  (filter-accumulate p? + 0 a (lambda (x) (+ x 1)) b))

#! zad13
(define (accumulate-13a p? a next b)
  (define (loop i)
        (cond ((> i b) #t)
          ((p? i) (loop (next i)))
          (#t #f)))
  (loop a))

(define (accumulate-13b p? a next b)
  (define (loop i)
    (cond ((> i b) #f)
          ((p? i) #t)
          (#t (loop (next i)))))
  (loop a))

(define (all? p? a b)
  (accumulate-13a p? a (lambda (x) (+ x 1)) b))

(define (any? p? a b)
  (accumulate-13b even? a (lambda (x) (+ x 1)) b))

#! zad14

(define (devider? num a)
  (if (= (remainder num a) 0)
      #t
      #f
      ))

(define (accumulate-14 p? a next b num)
  (define (loop i)
    (cond ((> i b) #t)
          ((p? num i) #f)
          (#t (loop (next i)))))
  (loop a))

(define (prime? num)
  (accumulate-14 devider? 2 (lambda (x) (+ x 1)) (- num 1) num))
