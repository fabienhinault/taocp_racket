#lang racket

(require mock)
(require rackunit)
(require mock/rackunit)
(require racket/generator)

(define (generate rs proc)
  (__generate rs (build-list (length rs) (λ (i) 0)) proc))

(define (__generate rs gd proc)
    (proc gd)
    (to-next rs gd 0 proc))

(define (to-next rs gd j proc)
  (when (not (equal? j (length rs)))
    (if (equal? (list-ref gd j) (sub1 (list-ref rs j)))
        (to-next rs (list-set gd j 0) (add1 j) proc)
        (__generate rs (list-set gd j (add1 (list-ref gd j))) proc))))

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



