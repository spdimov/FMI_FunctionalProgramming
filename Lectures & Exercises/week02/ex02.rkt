#! help functions
(define (++ n)
  (+ n 1))

(define (-- n)
  (- n 1))

(define (last-digit n)
  (remainder n 10))

(define (without-last-digit n)
  (quotient n 10))

#! zadacha 1

(define (count-digit d n)
  (define (loop d n cnt)
    (if (= d 0)
        cnt
        (if (= (lastDigit d) n)
            (loop (withoutLastDigit d) n (++ cnt))
            (loop (withoutLastDigit d) n cnt))))
  (loop d n 0))

#! zadacha 2

(define (fact n)
  (define (loop n acc)
    (if (= n 1)
        acc
        (loop (-- n) (* acc n))))
  (loop n 1))

#! zadacha 3

(define (fib n)
  (define (loop i result next)
    (if (= i n)
        result
        (loop (++ i) next (+ next result))))
  (loop 1 1 1))

#! zadacha 4

(define (reverse-int n)
  (define (loop n reverse)
    (if (<= n 0)
        reverse
        (loop (without-last-digit n) (+ (* reverse 10) (last-digit n)))))
  (loop n 0))

#! zadacha 5

(define (palindrome? n)
  (if (= (reverse-int n) n)
      #t
      #f))

#! zadacha 6

(define (devisors-sum n)
    (define (loop current sum)
      (if (> current (quotient n 2))
          sum
          (if (= (remainder n current) 0)
              (loop (++ current) (+ sum current))
              (loop (++ current) sum))))
    (loop 1 0))

#! zadacha 7

(define (perfect? n)
  (if (= (devisors-sum n) n)
      #t
      #f))

#! zadacha 8

(define (prime? n)
  (if (= (devisors-sum n) 1)
      #t
      #f))

#! zadacha 9

(define (increasing? n)
  (define (loop num prev)
    (if (= num 0)
        #t
        (if (> (last-digit num) prev)
            #f
            (loop (without-last-digit num) (last-digit num)))))
  (loop n 10))

#! zadacha 10

(define (toBinary n)
  (define (loop num mult bin)
    (if (= num 0)
        bin
        (loop (quotient num 2) (* 10 mult )(+ bin (* mult (remainder num 2))))))
  (loop n 1 0))

#! zadacha 11

(define (toDecimal n)
  (define (loop bin mult dec)
    (if (= bin 0)
        dec
        (loop (without-last-digit bin) (* 2 mult) (+ dec (* mult (last-digit bin))))))
  (loop n 1 0))