#lang racket

(define (square-sum a b)
  (+ (* a a) (* b b))
)

(define (max-two-square-sum a b c)
  (cond ((and (< a b) (< a c)) (square-sum b c))
        ((and (< c a) (< c b)) (square-sum a b))
        (else (square-sum a c))
  )
)

(equal? (max-two-square-sum 3 4 5) 41)
