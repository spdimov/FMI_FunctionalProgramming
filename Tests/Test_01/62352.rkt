;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 1
; 2020-11-07
;
; Начален час на контролното: 10:00
; Име: Станислав Димов
; ФН:62352
; Специалност:СИ
; Курс: 3
; Административна група: 5
;

;;zad1

(define (last-digit number)
  (remainder number 10))

(define (without-last-digit number)
  (quotient number 10))

(define (number-valid? n)
  (define (loop number flag)
    (cond ((= number 0) #t)
          ((and (= (last-digit number) 0) (eq? flag #t)) #f)
          ((and (= (last-digit number) 0) (eq? flag #f)) (loop (without-last-digit number) #t))
          (else (loop (without-last-digit number) #f))
          )
    )
  (loop n #f))

;;zad2

(define (left-shift set n)
  (* set (expt 2 n)))

(define (right-shift set n)
  (quotient set (expt 2 n)))

(define (drop-lower set n)
  (left-shift (right-shift set n) n))

(define (drop-higher-than set n)
  (- set (drop-lower set n)))

(define (set-remove set elem)
  (+ (drop-higher-than set elem)
     (drop-lower set (+ 1 elem))))

(define (set-add set elem)
  (+ (set-remove set elem)
     (left-shift 1 elem)))

(define (valid->nset n)
  (define (loop number curr set)
    (if (= number 0)
        (set-add set curr)
        (cond ((= (last-digit number) 0) (loop (without-last-digit number) 0 (set-add set curr)))
              (else (loop (without-last-digit number) (+ (* 10 curr) (last-digit number)) set))
              )
        )
    )
  (if (number-valid? n)
      (loop n 0 0)
      #f))

;;zad3

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (make-nset a b pred?)
  (define (op x y)
    (if (pred? x)
        (set-add y x)
        y))
       

  (define (id x) x)
  (define (next x) (+ x 1))
  
  (accumulate op id 0 a next b))
  

        
