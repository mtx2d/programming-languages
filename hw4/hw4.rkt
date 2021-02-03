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
    (if (= n 0) 
        null 
        (cons (car (s)) (stream-for-n-steps s (- n 1)))))

;; problem 5
(define (funny-number-stream)
    (let)
    (lambda () (cons (cond []) 
                        funny-number-stream))
)