#lang racket

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (square a)
  (* a a)
)

(define (good-enough? guess impoved-guess)
  (< (abs (- guess impoved-guess)) 0.00000001)
)

(define (sqrt-iter prev-guess guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter guess (improve guess x) x)
  )
)

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x)
)

(equal? (sqrt 4) 2.0)
