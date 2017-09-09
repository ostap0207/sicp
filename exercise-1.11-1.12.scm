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

; Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.
;
;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1
;        . . .

; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers
; above it. Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.

(define (pascal-triangel-element row n)
  (cond ((= n row) 1)
        ((= n 1) 1)
        (else (+ (pascal-triangel-element (- row 1) n)
                 (pascal-triangel-element (- row 1) (- n 1))))))

(equal? (pascal-triangel-element 5 3) 6)
(equal? (pascal-triangel-element 4 2) 3)
