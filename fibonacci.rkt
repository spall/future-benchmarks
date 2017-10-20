#lang racket/base

(require racket/future
         racket/cmdline)

#|
  How do I acurately time how long it takes to create a future? 

Any future intensive program is going to create a tree of futures, do work, at leaves, and
combine the results.

Goal: can use to compare futures on different systems. because program cost is mostly futures
but don't divide by count of futures..

How does it compare to sequential?

For fibonacci of n, how many futures are created?

nfc(0) = 0
nfc(1) = 0
nfc(2) = 0

nfc(n) = nfc(n-1) + nfc(n-2) + 2

Chart looks like:

y-axis is time. 
x-axis is #  of futures created

Plot of data for each system. 
System 1.  racket7 w/futures
System 1b. racket7 sequential
System 2.  racket w/futures
System 2b. racket sequential
System 4.  Haskell ?
System 5. .... 
|#

(define (sequential-fib n)
  (cond
    [(= 0 n)
     0]
    [(or (= 1 n) (= 2 n))
     1]
    [else
     (+ (sequential-fib (- n 1))
     	(sequential-fib (- n 2)))]))

(define (par-fib n)
  (cond
    [(= 0 n)
     0]
    [(or (= 1 n) (= 2 n))
     1]
    [else
     (let ([n-1 (future (lambda () (par-fib (- n 1))))]
     	   [n-2 (future (lambda () (par-fib (- n 2))))])
       (+ (touch n-1) (touch n-2)))]))

;; number of futures launched for dumb par fib.
(define (nfc n)
  (cond
    [(or (= 0 n) (= 1 n) (= 2 n))
     0]
    [else
     (+ 2 (nfc (- n 1)) (nfc (- n 2)))]))

(define-values (MAX file version)
  (command-line
   #:program "Fibonacci"
   #:args (n filename v)
   (values (string->number n) filename v)))

(define rfile (open-output-file file #:exists 'append))

(for ([i (in-range 1 (+ 1 MAX))])
  (let-values ([(_ scpu sreal sgc) (time-apply sequential-fib (list i))]
               [(__ pcpu preal pgc) (time-apply par-fib (list i))])
    ;; write these to file.
    ;; type i #-of-futures cpu real gc
    (let ([nf (nfc i)])
      (displayln (format "~a ~a ~a ~a ~a ~a" (string-append version "seq") i nf scpu sreal sgc) rfile)
      (displayln (format "~a ~a ~a ~a ~a ~a" (string-append version "par") i nf pcpu preal pgc) rfile))))

  
              
    
    
         





       
     