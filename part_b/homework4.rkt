
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs ))

(define (list-nth-mod xs n)
  (if (< n 0)
    (error "list-nth-mod: negative number")
    (if (null? xs)
        (error "list-nth-mod: empty list")
        (let ([size (length xs)]
              [i (remainder n (length xs))])
          (car (list-tail xs (- size (+ 1 i))))))))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([item (s)])
        (cons (car item) (stream-for-n-steps (cdr item) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5))
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x)))
                                (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec
      ([xl (length xs)]
       [yl (length ys)]
       [fh (lambda (x y) (remainder x y))]
       [f (lambda (x y) (cons (cons (car (list-tail xs (fh x xl))) (car (list-tail ys (fh y yl))))
                              (lambda () (f (+ 1 x) (+ 1 y)))))])
    (lambda () (f 0 0))))

(define (vector-assoc v vec)
  (letrec ([vl (vector-length vec)]
           [f (lambda (i)
                (cond [(>= i vl) #f]
                      [(and (pair? (vector-ref vec i))
                              (eq? v (car (vector-ref vec i))))
                       (vector-ref vec i)]
                      [#t (f (+ i 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec
      ([memo (make-vector n #f)]
       [i 0]
       [f (lambda (x)
          (let ([v (vector-assoc x memo)])
            (if v
                v
                (let ([new (assoc x xs)])
                  (if new
                    (begin (vector-set! memo i x)
                           (set! i (remainder (+ 1 i) n))
                            new)
                    #f)))))])
    f))