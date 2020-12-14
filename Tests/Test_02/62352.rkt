;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Станислав Димов
; ФН: 62352
; Специалност: СИ
; Курс: 3
; Административна група: 5
; Начален час на контролното: 9:45
;

#lang racket/base

(provide (all-defined-out))

(define left-tree cadr)
(define right-tree caddr)
(define root-tree car)

(define (weight node)
  (cond ((null? node) 0)
        (else (+ 1 (max (weight (left-tree node))
                        (weight (right-tree node)))))
        )
  )

(define (weight-balanced? tree)
  (cond ((null? tree) #t)
        ((< 1 (abs (- (weight (left-tree tree))
                      (weight (right-tree tree))))) #f)
        (else (and (weight-balanced? (left-tree tree))
                   (weight-balanced? (right-tree tree))))
        )
  )

(define (get-info-subject lst subj)
  (filter (lambda (x) (equal? (cadr x) subj)) lst)
  )

(define (get-fn lst)
  (map (lambda (x) (car x)) lst))

(define (uniques lst)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (= x z))) y))) '() lst))

                    
(define (make-pairs lst fn)
  (map (lambda (x) (list x (length
                            (filter
                             (lambda (y) (equal? (car y) x)) lst))))
       fn
       )
  )

(define (attempts subj res)
  (make-pairs (get-info-subject res subj)
              (uniques (get-fn res))
              )
  )