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
    (letrec ([helper (lambda (i) (cond 
                                    [(equal? (vector-length vec) i ) #f]
                                    [(pair? (vector-ref vec i)) (if (equal? v (car (vector-ref vec i))) 
                                                                    (vector-ref vec i)
                                                                    (helper (+ i 1)))]
                                    [#t (helper (+ i 1))]))])
    (helper 0)))

;; problem 10
(define (cached-assoc xs n)
    (let ([memo (make-vector n)] [next 0])
        (define (f x) 
            (let ([memo_lookup_res (vector-assoc x memo)])
                (if memo_lookup_res
                    memo_lookup_res
                    (let ([lst_lookup_res (assoc x xs)]) 
                        (if lst_lookup_res (begin (vector-set! memo next lst_lookup_res)  ;; set cache
                                                    (set! next (modulo (+ next 1) n))   ;; increment next
                                                    lst_lookup_res)
                                            #f)))))
        (lambda (v) (f v))))

; while-less: Macro terminates with #t as result (do: bad syntax   in: do) [error]
; while-less: Evaluates e2 the correct number of times (do: bad syntax   in: do) [error]
; while-less: Evaluates e1 only once (do: bad syntax   in: do) [error]
; vector-assoc: Lookup should succeed (Result of (vector-assoc 5 (vector (cons 1 2) (cons (quote blah) 2) (cons 5 3))) was expected to equal '(5 . 3)) [incorrect answer]
; vector-assoc: Lookup skips non-pairs (Result of (vector-assoc 5 (vector (quote blah) something (lambda () (quote blah)) (cons -5 3) (cons 5 2))) was expected to equal '(5 . 2)) [incorrect answer]
; vector-assoc: Empty vector (vector-ref: index is out of range for empty vector   index: 0) [error]
; cached-assoc: Full caching implementation (doesn't use assoc for elements in cache) [incorrect answer]
; cached-assoc: Checks cache for answer first before using assoc (submission uses assoc after finding element in cache) [incorrect answer]

; Because the auto-grader gave a score above 80, here is the link to a message from a very cute dog: https://drive.google.com/file/d/0B5sUgbs6aDNpSWhSZzVtcktDaTA/view?pref=2&pli=1



