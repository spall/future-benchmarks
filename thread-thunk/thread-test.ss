#! /u/sjspall/ChezScheme/bin/scheme --script
#|
  seeing how chez scheme threads perform when just running a thunk. 
  pretend future system.  

  lets just throw away the result for now. 
|#
(include "deque.ss")

(define die? #f)
(define run-count #f)
(define work-done (box 0))
(define main-thread-cond (make-condition))
(define worker-cond (make-condition))
(define worker-mutex (make-mutex))
(define main-thread-mutex (make-mutex))

(define (increment-work-done)
  (let ([old (unbox work-done)])
    (unless (box-cas! work-done old (+ 1 old))
            (increment-work-done))))

(define (terminate)
  (define curr (unbox run-count))
  (cond
   [(box-cas! run-count curr (- curr 1))
    (when (= 0 (- curr 1)) ;; last thread to terminate
	  (with-mutex main-thread-mutex
		      (condition-signal main-thread-cond)))
    (with-mutex worker-mutex
		(condition-wait worker-cond worker-mutex))]
   [else
    (terminate)]))

(define (unterminate)
  (define curr (unbox run-count))
  (unless (box-cas! run-count curr (+ curr 1))
	  (unterminate)))

(define work-queue (make-q))

(define (launch n)
  (set! run-count (box n))
  (for-each (lambda (x)
              (fork-thread future-scheduler))
            (iota n)))

(define (future-scheduler)
  (define (do-work work)
    (work))

  (let loop ([c 0])
    (define w (steal work-queue))
    (cond
     [(eq? w 'Abort)
      (loop c)]
     [(eq? w 'Empty)
      (terminate)
      (unless die?
	      (unterminate)
	      (loop c))
      (printf "Worker actually dying\n")]
     [else
      (do-work w)
      (increment-work-done)
      (loop (+ 1 c))])))

(define (add-thunk thunk)
  (push-bottom work-queue thunk))

;; on tcount pthreads thunk n times. 
(define (driver tcount n thunk)
  (set! die? #f)
  (launch tcount)
  
  
  (let loop ([nt n])
    (cond
     [(= 0 nt)
      (mutex-acquire main-thread-mutex)
      (if (= 0 (unbox run-count))
	  (printf "Done!\n")
	  (begin (printf "Main thread going to sleep; work done so far ~a\n" (unbox work-done))
		 (condition-wait main-thread-cond main-thread-mutex)
		 (mutex-release main-thread-mutex)
		 (set! die? #t)
		 (with-mutex worker-mutex
			     (condition-broadcast worker-cond)
                             (printf "Done!\n"))))]
     [else
      (add-thunk thunk)
      (when (< (unbox run-count) tcount) ;; some worker(s) went to sleep
             (with-mutex worker-mutex
			 ;(printf "waking up workers\n")
			 (condition-broadcast worker-cond)))
      (loop (- nt 1))])))

(define work (lambda ()
               (let loop ([n 30])
                 (cond
                  [(= n 0)
                   0]
                  [(or (= 1 n) (= 2 n))
                   1]
                  [else
                   (+ (loop (- n 1))
                      (loop (- n 2)))]))))      

(let ([args (cdr (command-line))])
  (cond
   [(null? args)
    (errorf 'thread-tests "Expected --thread-count and --number-thunks")]
   [else
    (let-values ([(tcount args)
		  (if (equal? (car args) "--thread-count")
		      (values (string->number (cadr args))
			      (cddr args))
		      (errorf 'thread-test "Expected --thread-count"))])
      (let-values ([(nthunks args)
		    (if (equal? (car args) "--number-thunks")
			(values (string->number (cadr args))
				(cddr args))
			(errorf 'thread-tests"Expected --number-thunks"))])
	(time (driver tcount nthunks work))))]))



;(time (driver 1 5000 work))

;(time (driver 2 5000 work))

;(time (driver 4 5000 work))
;(time (driver 8 5000 work))

;(time (driver 3 50000 work))

(exit)


                  


  
  
  
  
  

  
