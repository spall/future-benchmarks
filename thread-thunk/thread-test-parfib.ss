#! /u/sjspall/ChezScheme/bin/scheme --script

(include "deque.ss")

(define-record future* (eng flags result waiter lock)) ;; done? blocked?

(define (future*-done? f)
  (or (= (future*-flags f) 2) (= (future*-flags f) 3)))

(define (future*-blocked? f)
  (or (= (future*-flags f) 1) (= (future*-flags f) 3)))

(define (set-future*-done?! f v)
  (cond
   [(and v (future*-blocked? f))
    (set-future*-flags! f 3)]
   [v
    (set-future*-flags! f 2)]
   [(future*-blocked? f)
    (set-future*-flags! f 1)]
   [else
    (set-future*-flags! f 0)]))

(define (set-future*-blocked?! f v)
  (cond
   [(and v (future*-done? f))
    (set-future*-flags! f 3)]
   [v
    (set-future*-flags! f 1)]
   [(future*-done? f)
    (set-future*-flags! f 2)]
   [else
    (set-future*-flags! f 0)]))

(define workers #f) ;; vector of queues for stealing
(define workers-counts #f)

(define (make-lock)
  (box 0))

(define (acquire-lock l)
  (unless (box-cas! l 0 1)
	  (acquire-lock l)))

(define (release-lock l)
  (unless (box-cas! l 1 0)
	  (error 'release-lock "Failed to release lock")))

(define current-future (make-thread-parameter #f))

(define (thunk-wrapper thunk f)
  (lambda ()
    (let ([result (thunk)])
      (acquire-lock (future*-lock f))
      (set-future*-done?! f #t)
      (set-future*-result! f result)
      (release-lock (future*-lock f))
      ;(printf "A future is done its result is ~a\n" result)
      ;; wake up waiter
      (when (future*-waiter f)
	    (set-future*-blocked?! (future*-waiter f) #f)
	    (push-bottom (vector-ref workers (get-thread-id)) (future*-waiter f))))))

(define (future thunk)
  (define f (make-future* #f 0 (void) #f (make-lock)))
  (set-future*-eng! f (make-engine (thunk-wrapper thunk f)))
  (push-bottom (vector-ref workers (get-thread-id)) f)
  f)

(define (touch f)
  (acquire-lock (future*-lock f))
  (cond
   [(future*-done? f)
    (release-lock (future*-lock f))
    (future*-result f)]
   [(current-future)
    (set-future*-waiter! f (current-future))
    (release-lock (future*-lock f))
    (set-future*-blocked?! (current-future) #t)
    (engine-block)
    (unless (future*-done? f)
	    (errorf 'touch "Future awoken but what it touched is not done"))
    (future*-result f)]
   [else
    (release-lock (future*-lock f))
    ;; main thread wait on a condition.
    (touch f)]))


(define (par-fib n)
  (cond
   [(= n 0)
    0]
   [(or (= 1 n) (= 2 n))
    1]
   [else
    (let ([f1 (future (lambda () (par-fib (- n 1))))]
	  [f2 (future (lambda () (par-fib (- n 2))))])
      (+ (touch f1) (touch f2)))]))

(define (future-scheduler)
  (define TICKS 10000000000)
  (define complete (lambda (t v) (void)))
  ;; todo: fix me. don't run engine again if blocked.
  (define (expire f)   
    (lambda (new-eng)
      (cond
       [(future*-blocked? f)
	(set-future*-eng! f new-eng)]
       [else
	(new-eng TICKS complete (expire f))])))
  
  (define (do-work f)
    (current-future f)
    ((future*-eng f) TICKS complete (expire f))
    (vector-set! workers-counts (get-thread-id) (+ 1 (vector-ref workers-counts (get-thread-id))))
    (current-future #f))
    
  (define (steal-from-main)
    (define w (steal (vector-ref workers 0)))
    (cond
     [(eq? w 'Abort)
      (steal-from-main)]
     [else
      w]))

  (define (steal-from-peer)
    
    #;(define r (let rand ()
		(let ([r_ (+ 1 (random (- (vector-length workers) 1)))])
		  (if (or (= r_ (get-thread-id))
			  (empty? (vector-ref workers r_)))
		      (rand)
		      r_))))
    ;(printf "r is ~a\n" r)
    (let loop ([i 1])
      (cond
       [(>= i (vector-length workers))
	'Empty]
       [(= i (get-thread-id))
	(loop (+ 1 i))]
       [else
	;(printf "~a stealing from ~a\n" (get-thread-id) i)
	(let ([w (steal (vector-ref workers i))])
	  (cond
	   [(eq? w 'Abort)
	    ;(printf "got abort\n")
	    (loop i)]
	   [(eq? w 'Empty)
	    ;(printf "~a got empty from ~a\n" (get-thread-id) i)
	    (loop (+ 1 i))]
	   [else
	    w]))])))
	    
    (let loop ([c 0])
      (define w (pop-bottom (vector-ref workers (get-thread-id))))
      (cond
       [(eq? w 'Empty)
	(let ([w2 (steal-from-main)])
	  (cond
	   [(eq? w2 'Empty)
	    (let ([w3 (steal-from-peer)])
	      (cond
	       [(eq? w3 'Empty)
		;(printf "Got empty from peer\n")
		(loop c)]
	       [else
		;(printf "got work from peer\n")
		(do-work w3)
		(loop (+ 1 c))]))]
	   [else
	    (do-work w2)
	    (loop (+ 1 c))]))]
       [else
	(do-work w)
	(loop (+ 1 c))])))

(define (launch n)
  (set! workers (make-vector (+ 1 n)))
  (set! workers-counts (make-vector (+ 1 n) 0))
  (vector-set! workers 0 (make-q)) ;; main thread queue
  (for-each (lambda (x)
	      (vector-set! workers (+ 1 x) (make-q))
	      (fork-thread future-scheduler))
	    (iota n)))

(define (driver tcount thunk)
  (set! die? #f)
  (launch tcount)
  (let ([f (future thunk)])
    (printf "Result: ~a\n" (touch f))
    (for-each (lambda (x)
		(printf "Worker ~a did ~a work\n" (+ 1 x) (vector-ref workers-counts (+ 1 x))))
	      (iota tcount))))

(define (par-fib n)
  (cond
   [(= n 0)
    0]
   [(or (= 1 n) (= 2 n))
    1]
   [else
    (let ([f1 (future (lambda () (par-fib (- n 1))))]
	  [f2 (future (lambda () (par-fib (- n 2))))])
      #;(let busy ([c 5000000])
	(when (>= c 0)
	      (busy (- c 1))))
      (+ (touch f1) (touch f2)))]))

(printf "Result: ~a\n" (par-fib 2))

(time (driver 4 (lambda () (par-fib 30))))

#;(let ([args (cdr (command-line))])
  (cond
   [(null? args)
    (errorf 'thread-tests "Expected --thread-count")]
   [else
    (let-values ([(tcount args)
		  (if (equal? (car args) "--thread-count")
		      (values (string->number (cadr args))
			      (cddr args))
		      (errorf 'thread-test "Expected --thread-count"))])
      (time (driver tcount nthunks work)))]))


;(time (driver 4 (lambda () (par-fib 30))))

;(time (driver 8 (lambda () (par-fib 30))))

;(time (driver 16 (lambda () (par-fib 30))))


