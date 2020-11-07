#! zad2

(define (last-digit n)
  (remainder n 10))

(define (without-last-digit n)
  (quotient n 10))

(define (accumulate-i init op a next b)
  (define (loop result i)
    (if (<= i b)
        (loop (op result i) (next i))
        result
        ))
  (loop init a))

(define (pow number power)
  (accumulate-i 1 (lambda (x y) (* x number))  1 (lambda (x) (+ x 1)) power))

(define (toBinary set)
  (string->number (number->string set 2)))

(define (toDecimal set)
  (string->number (string-append "#b"(number->string set))))

(define (set-contains? set elem)
  (define (loop i set)
    (if (= i elem)
        (if (= (last-digit set) 1)
            #t
            #f)
        (loop (+ i 1) (without-last-digit set))
        )
    )
  (loop 0 (toBinary set)))
        
(define (set-add set elem)
  (if (set-contains? set elem)
      set
      (toDecimal (+ (toBinary set) (pow 10 elem)))))

(define (set-remove set elem)
  (if (set-contains? (toBinary set) elem)
      (toDecimal (- (toBinary set) (pow 10 elem)))
      set))

(define (set-empty? set)
  (if (= set 0)
      #t
      #f))

(define (set-size set)
  (define (loop set result)
    (if (= set 0)
        result
        (if (= (last-digit set) 1)
        (loop (without-last-digit set) (+ result 1))
        (loop (without-last-digit set) result))))
  
  (loop (toBinary set) 0))

  (define (set-intersect s1 s2)
    (define (loop mult result s1 s2)
      (if (or (= s1 0) (= s2 0))
          (toDecimal result)
          (if (and (= (last-digit s1) 1) (= (last-digit s2) 1))
              (loop (* 10 mult) (+ result mult) (without-last-digit s1) (without-last-digit s2))
              (loop (* 10 mult) result (without-last-digit s1) (without-last-digit s2)))))

    (loop 1 0 (toBinary s1) (toBinary s2)))

(define (set-union s1 s2)
    (define (loop mult result s1 s2)
      (if (or (= s1 0) (= s2 0))
          (toDecimal result)
          (if (or (= (last-digit s1) 1) (= (last-digit s2) 1))
              (loop (* 10 mult) (+ result mult) (without-last-digit s1) (without-last-digit s2))
              (loop (* 10 mult) result (without-last-digit s1) (without-last-digit s2)))))

    (loop 1 0 (toBinary s1) (toBinary s2)))

(define (set-difference s1 s2)
    (define (loop mult result s1 s2)
      (if (or (= s1 0) (= s2 0))
          (toDecimal result)
          (if (and (= (last-digit s1) 1) (= (last-digit s2) 0))
              (loop (* 10 mult) (+ result mult) (without-last-digit s1) (without-last-digit s2))
              (loop (* 10 mult) result (without-last-digit s1) (without-last-digit s2)))))

    (loop 1 0 (toBinary s1) (toBinary s2)))

(define (set-value set)
  (define (loop pos result set)
    (if (= set 0)
        result
        (if (= (last-digit set) 1)
            (loop (+ pos 1) (+ result (p pos)) (without-last-digit set))
            (loop (+ pos 1) result (without-last-digit set) )
            )))
  (loop 0 0 (toBinary set)))

(define (max-set set1 set2)
  (if (> (set-value (toBinary set1)) (set-value (toBinary set2)))
      set1
      set2))

(define (knapsack capacity n w p)
  (define (recursion c pos set)
    (if (or (> pos n) (= c 0))
        set
        (if (> (w pos) c)
            (recursion c (+ pos 1) set)
            (max-set (recursion (- c (w pos)) (+ pos 1) (set-add set pos))
                     (recursion c (+ pos 1) set)))))
  (recursion capacity 1 0))



   
                                                                      
                                                    
                                                        
                                                        
             
  
 