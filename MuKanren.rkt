#lang racket

(define (var name)
  (vector name))

(define (var? x)
  (vector? x))

(define empty-s `())

(define (walk v s) 
  (let ([a (and (var? v) (assv v s))])
    (if (pair? a) (walk (cdr a) s) v)))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(define (ext-s x v s)
  (if (occurs? x v s) #f (cons `(,x . ,v) s)))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(eqv? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [else #f])))

(define (== u v)
  (lambda (s)
    (let ([s (unify u v s)])
      (if s `(,s) `()))))

(define suc
  (lambda (s)
    `(,s)))

(define fail
  (lambda (s)
    `()))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(define (append-inf s-inf t-inf)
  (cond
    [(null? s-inf) t-inf]
    [(pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf))]
    [else (thunk (append-inf t-inf (s-inf)))]))

(define (nevero)
  (lambda (s)
    (thunk ((nevero) s))))

(define (alwayso)
  (lambda (s)
    (thunk ((disj2 suc (alwayso)) s))))

(define (take-inf n s-inf)
  (cond
    [(and n (zero? n) `())]
    [(null? s-inf) `()]
    [(pair? s-inf)
     (cons (car s-inf)
           (take-inf (and n (sub1 n)) (cdr s-inf)))]
    [else (take-inf n (s-inf))]))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(define (append-map-inf g s-inf)
  (cond
    [(null? s-inf) `()]
    [(pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf)))]
    [else (thunk (append-map-inf g (s-inf)))]))

(define (call/fresh name f)
  (f (var name)))

(define (reify-name n)
  (string->symbol
   (string-append "-" (number->string n))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v)
       (let ([n (length r)])
         (let ([rn (reify-name n)])
           (cons `(,v . ,rn) r)))]
      [(pair? v)
       (let ([r (reify-s (car v) r)])
         (reify-s (cdr v) r))]
      [else r])))

(define (reify v)
  (lambda (s)
    (let ([v (walk* v s)])
      (let ([r (reify-s v empty-s)])
        (walk* v r)))))

(define (run-goal n g)
  (take-inf n (g empty-s)))

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj <g>) <g>]
    [(disj <g0> <g1> ...) (disj2 <g0> (disj <g1> ...))]))

(define-syntax conj
  (syntax-rules ()
    [(conj) suc]
    [(conj <g>) <g>]
    [(conj <g0> <g1> ...) (conj2 <g0> (conj <g1> ...))]))

(define-syntax defrel
  (syntax-rules ()
    [(defrel (<name> <arg> ...) <g> ...)
     (define (<name> <arg> ...)
       (lambda (s)
         (thunk ((conj <g> ...) s))))]))

(define-syntax run
  (syntax-rules ()
    [(run <n> (<x0> <x1> ...) <g> ...)
     (run <n> q (fresh (<x0> <x1> ...)
                       (== `(,<x0> ,<x1> ...) q) <g> ...))]
    [(run <n> <q> <g> ...)
     (let ([<q> (var (quote <q>))])
       (map (reify <q>)
            (run-goal <n> (conj <g> ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(run* <q> <g> ...) (run #f <q> <g> ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(fresh () <g> ...) (conj <g> ...)]
    [(fresh (<x0> <x1> ...) <g> ...)
     (call/fresh (quote <x0>)
                 (lambda (<x0>)
                   (fresh (<x1> ...) <g> ...)))]))
(define-syntax conde
  (syntax-rules ()
    [(conde (<g> ...) ...)
     (disj (conj <g> ...) ...)]))

(provide run* run conde defrel suc fail conj disj == fresh)