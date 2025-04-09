#lang racket
(provide comment)

(define-syntax-rule (comment body ...)
  (void))

(define (insert-at i x xs)
  (let loop ((i i)
             (vs '())
             (xs xs))
    (cond
      ((zero? i) (append vs `(,x) xs))
      ((loop (- i 1) (cons (car xs) vs) (cdr xs))))))
