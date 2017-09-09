#lang racket

; Exercise 1.11:
; A function ff is defined by the rule that
; f(n)=nf(n)=n if n<3n<3 and f(n)=f(n−1)+2f(n−2)+3f(n−3)f(n)=f(n−1)+2f(n−2)+3f(n−3) if n≥3n≥3.
; Write a procedure that computes ff by means of a recursive process.
; Write a procedure that computes ff by means of an iterative process.

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (= count 2)
        a
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

(equal? (f-rec 10) (f-iter 10))
