#lang racket

; Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s
; Rule, the integral of a function ff between aa and bb is approximated as h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn),
; h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn), where h=(b−a)/nh=(b−a)/n, for some even integer nn, and yk=f(a+kh)yk=f(a+kh).
; (Increasing nn increases the accuracy of the approximation.) Define a procedure that takes as arguments ff, aa, bb, and
; nn and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0
; and 1 (with n=100n=100 and n=1000n=1000), and compare the results to those of the integral procedure shown above.


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (integral-by-simon f a b n)
  (define h (/ (- b a) n))
  (define (calc-y k) (f (+ a (* k h))))
  (define (calc-coef k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))

  (define (calc-element-at k) (* (calc-coef k) (calc-y k)))
  (* (sum calc-element-at 0 inc n) (/ h 3)))

(define (cube x) (* x x x))

(display (integral-by-simon cube 0 1 1000))
