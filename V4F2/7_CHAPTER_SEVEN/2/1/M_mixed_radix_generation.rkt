#lang racket

; install mock and mock-rackunit using "File > Package Manager ..." or raco pkg install
(require mock)
(require rackunit)
(require mock/rackunit)
(require racket/generator)

; apply proc to each list (a_1 ... a_n) where 0 <= a_i < m_i for i = 1, ..., n
; with radices = (m_1 ... m_n)
;> (generate '(1 2 3) println)
;'(0 0 0)
;'(0 1 0)
;'(0 0 1)
;'(0 1 1)
;'(0 0 2)
;'(0 1 2)
(define (generate radices proc)
  (__generate radices (build-list (length radices) (λ (i) 0)) proc))

(define (__generate radices generated proc)
    (proc generated)
    (to-next radices generated 0 proc))

(define (to-next radices generated j proc)
  (when (not (equal? j (length radices)))
    (if (equal? (list-ref generated j) (sub1 (list-ref radices j)))
        (to-next radices (list-set generated j 0) (add1 j) proc)
        (__generate radices (list-set generated j (add1 (list-ref generated j))) proc))))

;attempt using generator
(define g
  (generator
   (radices)
   (let loop ([rs radices] [gd  (build-list (length radices) (λ (i) 0))])
     (yield gd)
     (let to-next ([rs rs] [gd gd] [j 0])
       (when (not (equal? j (length rs)))
         (if (equal? (list-ref gd j) (sub1 (list-ref rs j)))
             (to-next rs (list-set gd j 0) (add1 j))
             (loop rs (list-set gd j (add1 (list-ref gd j))))))))))
       
;(time (generate '(2 9 5 3 4 6) println))
;cpu time: 3056 real time: 3054 gc time: 486
;(time (generate '(7 2 9 5 3 4 6) println))
;cpu time: 23187 real time: 23157 gc time: 3130
; ^ cpu time: 22851 real time: 22822 gc time: 3113

(let* ((void-mock (mock #:behavior void)))
  (to-next '(1) '(0) 1 void-mock)
  (check-mock-num-calls void-mock 0))

(let* ((void-mock (mock #:behavior void)))
  (to-next '(1) '(0)  0 void-mock)
  (check-mock-num-calls void-mock 0))

(let* ((void-mock (mock #:behavior void)))
  (__generate '(1) '(0)  void-mock)
  (check-mock-calls void-mock (list (arguments '(0)))))

(let* ((void-mock (mock #:behavior void)))
  (generate '(1) void-mock)
  (check-mock-calls void-mock (list (arguments '(0)))))

(let* ((void-mock (mock #:behavior void)))
  (generate '(2) void-mock)
  (check-mock-calls void-mock (list (arguments '(0)) (arguments '(1)))))

(let* ((void-mock (mock #:behavior void)))
  (generate '(3) void-mock)
  (check-mock-calls void-mock (list (arguments '(0)) (arguments '(1)) (arguments '(2)))))

(let* ((void-mock (mock #:behavior void)))
  (generate '(2 2) void-mock)
  (check-mock-calls void-mock (list (arguments '(0 0)) (arguments '(1 0)) (arguments '(0 1))  (arguments '(1 1)) )))

(let* ((void-mock (mock #:behavior void)))
  (generate '(2 3) void-mock)
  (check-mock-calls void-mock (list (arguments '(0 0)) (arguments '(1 0))
                                    (arguments '(0 1)) (arguments '(1 1))
                                    (arguments '(0 2)) (arguments '(1 2)))))

(let* ((void-mock (mock #:behavior void)))
  (generate '(3 2) void-mock)
  (check-mock-calls void-mock (list (arguments '(0 0)) (arguments '(1 0)) (arguments '(2 0))
                                    (arguments '(0 1)) (arguments '(1 1)) (arguments '(2 1)))))



