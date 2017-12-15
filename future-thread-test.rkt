#lang racket/base

(require racket/future
         racket/cmdline
         racket/list)


(define (sequential-fib n)
  (cond
    [(= 0 n)
     0]
    [(or (= 1 n) (= 2 n))
     1]
    [else
     (+ (sequential-fib (- n 1))
     	(sequential-fib (- n 2)))]))

(define (flat-fib n count)
  (define futures 
    (let loop ([ct count])
      (cond
        [(= 0 ct)
         '()]
        [else
         (cons (future (lambda () (sequential-fib n)))
               (loop (- ct 1)))])))
  (for-each touch futures))

(time (flat-fib 30 5000))
