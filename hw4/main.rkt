#lang racket
(provide (all-defined-out))

; basic definitions
(define s "hello")

(define x 3) ; val x = 3
(define y (+ x 2)); val y = x + 2

(define cube1 
    (lambda (x)
        (* x (* x x)))) ; x * (x * x)

(define cube2 (lambda (x) (* x x x)))

(define (cube3 x) (* x x x))

(define (pow1 x y) ; x to the yth power (y must be nonnegative)
    (if (= y 0) 
        1 
        (* x (pow1 x (- y 1)))))

(define pow2 (lambda (x) (lambda (y) (pow1 x y))))

; Empty list: null
; Cons constructor: cons
; Head of a list: car 
; Accese tail of a list: cdr
; Check for empty: null?

; append
; use Thunk to delay computation that I do not need

(define (my-delay thunk)
    (mcons #f thunk))

;; promise is a pair, first one is (false, thunk) or (true, (thunk))
(define (my-force p)
    (if (mcar p) 
        (mcdr p)
    (begin  (set-mcar! p #t)
            (set-mcdr p ((mcdr p))) 
            (mcdr p))))