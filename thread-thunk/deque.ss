

(define-record cwsdeque (top bottom array))

;; fix name
(define-record circular-array (log-size segment))
                                  
(define (make-ca log-size)
  (make-circular-array log-size (make-vector (ash 1 log-size)
                                        0)))

(define (grow-array a b t)
  (define na (make-ca (+ (circular-array-log-size a) 1)))
  (for-each (lambda (i)
              (array-put na i (array-get a i)))
            (filter (lambda (x)
                      (+ t x))
                    (iota (- b t))))
  na)

(define (array-size a)
  (ash 1 (circular-array-log-size a)))

(define (array-get a pos)
   (vector-ref (circular-array-segment a)
              (modulo pos (array-size a))))

(define (array-put a pos val)
  (vector-set! (circular-array-segment a)
               (modulo pos (array-size a))
               val))

;; q old new
(define cas-top! box-cas!)

(define INIT-LOG-SIZE 16)

(define (make-q)
  (make-cwsdeque (box 0) 0 (make-ca INIT-LOG-SIZE)))

;; invoked only by q owner
(define (push-bottom q w)
  (define b (cwsdeque-bottom q))
  (define t (unbox (cwsdeque-top q)))
  (define a (cwsdeque-array q))

  (define size (- b t))
  (when (>= size (- (array-size a) 1))
    (set! a (grow-array a b t))
    (set-cwsdeque-array! q a))

  (array-put a b w)
  (set-cwsdeque-bottom! q (+ b 1)))

;; invoked only by q owner
(define (pop-bottom q)
  (define b (cwsdeque-bottom q))
  (define a (cwsdeque-array q))
  (set! b (- b 1))
  (set-cwsdeque-bottom! q b)
  (let ([t (unbox (cwsdeque-top q))])
    (let ([size (- b t)])
      (cond
       [(< size 0)
        (set-cwsdeque-bottom! q t)
        'Empty]
       [else
        (let ([o (array-get a b)])
          (unless (> size 0)
                  (unless (cas-top! (cwsdeque-top q) t (+ t 1))
                          (set! o 'Empty))
                  (set-cwsdeque-bottom! q (+ t 1)))
        o)]))))
     
(define (steal q)
  (define t (unbox (cwsdeque-top q)))
  (define b (cwsdeque-bottom q))
  (define a (cwsdeque-array q))

  (define size (- b t))
  (cond
    [(<= size 0)
     'Empty]
    [else
     (let ([o (array-get a t)])
       (if (cas-top! (cwsdeque-top q) t (+ t 1))
           o
           'Abort))]))
      

