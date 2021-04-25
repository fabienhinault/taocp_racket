#lang racket

; install mock and mock-rackunit using "File > Package Manager ..." or raco pkg install
(require mock)
(require rackunit)
(require mock/rackunit)

; apply proc to all lists representing binary n-tuples starting from (0), changing
; one bit at a time
; (gray 3 println)
;'(0 0 0)
;'(1 0 0)
;'(1 1 0)
;'(0 1 0)
;'(0 1 1)
;'(1 1 1)
;'(1 0 1)
;'(0 0 1)
(define (gray max-length proc)
  (__gray (build-list max-length (λ (i) 0)) 0 max-length proc))

(define (__gray generated a-infinite max-length proc)
  (proc generated)
  (to-next generated a-infinite max-length proc))

(define (to-next generated a-infinite max-length proc)
  (let* ((a-infinite (- 1 a-infinite))
         (j (if (equal? a-infinite 1)
                0
                (__after-first-one generated 1))))
    (when (not (equal? j max-length))
      (__gray (list-set generated j (- 1 (list-ref generated j))) a-infinite max-length proc))))

(define (__after-first-one l i)
  (if (equal? (car l) 1)
      i
      (__after-first-one (cdr l) (add1 i))))

(check-equal? (__after-first-one '(1 0 0) 1) 1)
(check-equal? (__after-first-one '(0 1) 1) 2)
(check-equal? (__after-first-one '(0 0 1) 1) 3)
(check-equal? (__after-first-one '(0 0 1 0) 1) 3)
(check-equal? (__after-first-one '(0 0 0 1) 1) 4)

