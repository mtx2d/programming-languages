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
    (cond   [(negative? n) (error "list-nth-mod: negative number")]
            [(null? xs) (error "list-nth-mod: empty list")]
            [#t (car (list-tail xs (remainder n (length xs))))]))

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
        (cons   x 
                (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
        (f "dan.jpg")))

;; problem 7
(define (stream-add-zero s)
    (let* ([pr (s)] [item (car pr)] [next_s (cdr pr)])
        (lambda () (cons (cons 0 item) (stream-add-zero next_s)))))

;; problem 8
(define (cycle-lists xs ys)
    (letrec ([g (lambda (xs ys n) 
                (cons   (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                        (lambda() (g xs ys (+ n 1)))))])
                (lambda () (g xs ys 0))))

;; problem 9
(define (vector-assoc v vec)
    (letrec ([helper (lambda (s) (if (pair? (vector-ref vec s)) 
                            (cond [(equal? (vector-length vec) (+ s 1)) #f]
                                [(equal? v (car (vector-ref vec s))) (vector-ref vec s)]
                                [#t (helper (+ s 1))] ) 
                            (helper (+ s 1))))]) 
        (helper 0)))