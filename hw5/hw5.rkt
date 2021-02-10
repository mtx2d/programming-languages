;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1-a
(define (racketlist->mupllist es)
  (if (null? es) (aunit)
                 (apair (car es) (racketlist->mupllist (cdr es)))))

;; Problem 1-b
(define (mupllist->racketlist xs)
  (if (aunit? xs) null
                 (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2)) 
               (if (> (int-num v1) (int-num v2)) 
                 (eval-under-env (ifgreater-e3 e) env)
                 (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([v1 (eval-under-env (mlet-e e) env)]
               [new-env (cons (cons (mlet-var e) v1) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(closure? e) e]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
              [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
                (let* ([formal (fun-formal (closure-fun (call-funexp e)))]
                      [nameopt (fun-nameopt (closure-fun (call-funexp e)))]
                      [cenv (closure-env (call-funexp e))]
                      [aug-cenv (if nameopt 
                                  (cons (cons nameopt v1) (cons formal v2) cenv) 
                                  (cons (cons formal v2) cenv))])
                  (eval-under-env (fun-body (closure-fun (call-funexp e))) aug-cenv))
                (error "MUPL call applied to non-closure")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e)) (eval-under-env (apair-e2 e)))]
        [(fst? e)
          (if (apair? (fst-e e)) 
              (apair-e2 (fst-e e))
              (error "MUPL fst applied to non-pair"))]
        [(snd? e)
          (if (apair? (snd-e e)) 
              (apair-e2 (snd-e e))
              (error "MUPL snd applied to non-pair"))]
        [(isaunit? e)
          (if (aunit? (isaunit-e e)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (if (equal? (aunit) (eval-exp e1)) 
                                (eval-exp e2) (eval-exp e3)))

(define (mlet* lstlst e2) 
  (letrec ([get-env (lambda (lst accu) 
                      (if (null? lst)
                          accu
                          (let* ([pr (car lst)][var-name (car pr)][ex (cdr pr)]
                                [new-binding (cons var-name (eval-under-env ex accu))])
                            (get-env (cdr lst) (cons new-binding accu)))))])
                              (eval-under-env e2 (get-env lstlst null))))

(define (ifeq e1 e2 e3 e4) 
  (let ([v1 (eval-exp e1)][v2 (eval-exp e2)])
    (if (and (int? v1) (int? v2)) 
      (eval-exp 
        (if (equal? (int-num v1) (int-num v2)) 
          (mlet (var "_x") v1 (mlet (var "_y") v2 e3))
          (mlet (var "_x") v1 (mlet (var "_y") v2 e4))))
      (error "MUPL ifeq applied to non number"))))

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
