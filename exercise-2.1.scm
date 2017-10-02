#lang racket

;Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the
;sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational
;number is negative, only the numerator is negative.

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((new-n (/ n g))
          (new-g (/ d g)))
      (cons (* new-n (sgn new-g))
            (* new-g (sgn new-g))))))

(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
