#lang racket

; install mock and mock-rackunit using "File > Package Manager ..." or raco pkg install
(require mock)
(require rackunit)
(require mock/rackunit)
(require srfi/13) ; string library

(define a-bits (char->integer #\a))

(define (letter-bits char)
  (- (char->integer char) a-bits))

(define (word-number word)
  (let* ((chars (string->list word)))
    (list-number chars (sub1 (length chars)))))

(define (list-number chars k)
  (if (null? chars)
      0
      (+
       (* (expt 2 (* 5 k)) (letter-bits (car chars)))
       (list-number (cdr chars) (sub1 k)))))

(check-equal? (list-number '() 123) 0)
(check-equal? (list-number '(#\z) 0) 25)
(check-equal? (list-number '(#\z #\y) 1) (+ (* 25 (expt 2 5)) 24))
(check-equal? (word-number "zy") (+ (* 25 (expt 2 5)) 24))



; apply proc to all lists representing binary n-tuples starting from (0), changing
; one bit at a time
; (loopless-gray 3 println)
;'(0 0 0)
;'(1 0 0)
;'(1 1 0)
;'(0 1 0)
;'(0 1 1)
;'(1 1 1)
;'(1 0 1)
;'(0 0 1)
(define (loopless-gray max-length proc)
  (__loopless-gray (build-list max-length (λ (i) 0)) (build-list (add1 max-length) (λ (i) i)) max-length proc))

(define (__loopless-gray generated focus-pointers max-length proc)
  (proc generated)
  (to-next generated focus-pointers max-length proc))

(define (to-next generated focus-pointers max-length proc)
  (let* ((j (car focus-pointers))
         (j+1 (add1 j)))
    (when (not (equal? j max-length))
      (__loopless-gray (list-set generated j (- 1 (list-ref generated j)))
                       (list-set
                        (list-set
                         (list-set focus-pointers 0 0)
                         j (list-ref focus-pointers j+1))
                        j+1 j+1)
                       max-length proc))))


(let* ((void-mock (mock #:behavior void)))
  (loopless-gray 1 void-mock)
  (check-mock-num-calls void-mock 2)
  (check-mock-calls void-mock (list (arguments '(0)) (arguments '(1)))))

(let* ((void-mock (mock #:behavior void)))
  (loopless-gray 2 void-mock)
  (check-mock-num-calls void-mock 4)
  (check-mock-calls void-mock (list (arguments '(0 0))
                                    (arguments '(1 0))
                                    (arguments '(1 1))
                                    (arguments '(0 1)))))
