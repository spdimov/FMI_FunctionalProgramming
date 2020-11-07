(define (simple-op op term a next b)
  (define (loop i)
    (if (> i b)
        (begin (op (term i))
               (loop (next i)) ))
    )
  (loop a)
)

(define (squares square-number)
  
  (define (top n)
  (display "\u250C")
  (define (loop i)
    (if (= i (- (* 4 n) 3))
        (begin
          (display "\u2510")
          (right-side n)
           )
        (begin
          (display "\u2500")
          (loop (+ i 1))
          )
      )
    )
  (loop 0)
  )

(define (bottom n)
  (display "\u2514")
  (define (loop i)
    (if (= i (- (* 4 n) 3))
        (begin
          (display "\u2518")
          (right-side n)
           )
        (begin
          (display "\u2500")
          (loop (+ i 1))
          )
      )
    )
  (loop 0)
  )
  

  (define (right-side n)
         (define (loop i)
             (if (< i (- square-number n))
               (begin
                 (display " ")
                 (display "\u2502")
                      (loop (+ i 1))
               )
           )
           )
    (loop 0))

  (define (top-bottom type n) ;; започваме от ляво на дясно, като има значение в коя половина на квадрата сме
  (define (left-side n)
         (define (loop i)
             (if (= i (- square-number n))
               (begin
                 (type n)
                 (display "\n")
                 )
               (begin
                 (display "\u2502")
                 (display " ")
                 (loop (+ i 1))
               )
           )
           )
    (loop 0))
    (left-side n))

  
  (simple-op (lambda (x) (top-bottom top x))  (lambda (x) x) square-number (lambda (x) (- x 1)) 0)
  (simple-op (lambda (x) (top-bottom bottom x)) (lambda (x) (+ (abs (- x square-number)) 1)) square-number (lambda (x) (- x 1)) 0)
  )




  
