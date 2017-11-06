#lang racket

(define (square x) (* x x))

(define (square-tree l)
  (cond ((null? l) '())
        ((not (pair? l)) (square l))
        (else (cons (square-tree (car l)) (square-tree (cdr l))))))

(equal? (square-tree '(1 (2 (3 4) 5) (6 7)))
        '(1 (4 (9 16) 25) (36 49)))
