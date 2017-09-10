#lang racket

; Exercise 1.16:
; Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic
; number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2=(b2)n/2(bn/2)2=(b2)n/2,
; keep, along with the exponent nn and the base bb, an additional state variable aa, and define the state transformation
; in such a way that the product abnabn is unchanged from state to state.
; At the beginning of the process aa is taken to be 1, and the answer is given by the value of aa at the end of the process.
; In general, the technique of defining an invariant quantity that remains unchanged from state to state is a
; powerful way to think about the design of iterative algorithms.)

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (fast-iter-expt b n)
  (define (expt-iter acc b counter)
    (cond ((= counter 1) acc)
          ((even? counter) (expt-iter (square acc)
                                b
                                (/ counter 2)))
          (else (expt-iter (* (square acc) b)
                                b
                                (/ (- counter 1) 2)))))
  (expt-iter b b n))

(fast-iter-expt 2 4)
