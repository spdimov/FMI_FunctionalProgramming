(define (str-chr? str c) ;;проверява дали символът c се съдържа в низа str
  (define (loop i)
    (if (>= i (string-length str))
        #f
        (if (eqv? (string-ref str i) c)
            #t
            (loop (+ i 1))
            )
        )
    )
  (loop 0))

(define (str-sub-iter str a b) ;;Връща нов низ, който е частта от str, намираща се между позициите a и b.
  (define (loop substring i)
    (if (> i b)
        substring
        (loop (string-append substring (make-string 1 (string-ref str i))) (+ i 1))
           )
    )
  (loop "" a))


(define (str-sub-rec str a b)
  (if (< b a) ""
      (string-append
       (str-sub-rec str a (- b 1))
       (make-string 1 (string-ref str b)))))

(define (str-str? needle haystack) ;;Ако низът needle се съдържа в низа haystack, връща първата позиция, на която се среща. Иначе, връща #f.
  (define (loop haystack c)
    (if (< (string-length haystack) (string-length needle)) #f
        (if (equal?
             needle
             (str-sub-rec haystack 0 (- (string-length needle) 1))
             ) c
               (loop (str-sub-rec haystack 1 (- (string-length haystack) 1)) (+ c 1)))      
        )
    )
  (loop haystack 0))