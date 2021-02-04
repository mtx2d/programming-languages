#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; problem 1
(define (sequence low high stride)
    (if (> low high) 
        null
        (cons low (sequence (+ low stride) high stride))))

;; problem 2
(define (string-append-map xs suffix)
    (map (lambda (x) (string-append x suffix)) xs))

;; problem 3
(define (list-nth-mod xs n)
    (cond   [negative? n] (error "list-nth-mod: negative number")
            [null? xs] (error "list-nth-mod: empty list")
            [#t] (car (list-tail xs (remainder (length xs) n)))))

;; problem 4
(define (stream-for-n-steps s n)
    (let ([pr (s)])
        (if (= n 0) 
            null 
            (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; problem 5
(define (funny-number-stream)
    (letrec ([f (lambda (x) 
                (cons   (if (= (modulo x 5) 0) (- x) x) 
                        (lambda () (f (+ x 1)))))])
        (f 1)))
;; Stream is a thunk, when you call it returns a pair

;; problem 6
(define (dan-then-dog)
    (letrec ([f (lambda(x)
        (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dog.jpg")))))])
        (f "dan.jpg")))