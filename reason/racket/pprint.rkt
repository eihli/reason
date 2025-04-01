#lang racket/base

(provide pp/state
         pp/stream
         pp/goal)

(require (only-in racket/match match)
         first-order-miniKanren/microk-fo
         first-order-miniKanren/mk-fo
         first-order-miniKanren/tools
         racket/serialize)

(define (spaces i)
  (make-string i #\space))

(define (indent i)
  (+ i 2))

(define (pp/mplus i s1 s2)
  (format "mplus~n~a~a"
          (pp/stream (indent i) s1)
          (pp/stream (indent i) s2)))

(define (pp/bind i s g)
  (format "bind~n~a~a"
          (pp/stream (indent i) s)
          (pp/goal (indent i) g)))

(define (pp/disj i g1 g2)
  (format "disj~n~a~a"
          (pp/goal (indent i) g1)
          (pp/goal (indent i) g2)))

(define (flatten/conj c)
  (match c
    ((conj g1 g2)
     (cond
       ((and (conj? g1) (conj? g2))
        (append (flatten/conj g1)
                (flatten/conj g2)))
       ((conj? g1)
        (cons g2 (flatten/conj g1)))
       ((conj? g2)
        (cons g1 (flatten/conj g2)))
       (else (list g1 g2))))
    (c (list c))))

(define (pp/conj- i g1 g2)
  (format "conj~n~a~a"
          (pp/goal (indent i) g1)
          (pp/goal (indent i) g2)))

(define (pp/conj i g1 g2)
  (let ((cxs (flatten/conj (conj g1 g2))))
    (let loop ((cxs cxs)
               (out (format "Constraints:~n")))
      (if (null? cxs)
          out
          (loop (cdr cxs) (string-append out (pp/goal (indent i) (car cxs))))))))

(define (pp/== i t1 t2)
  (format "== ~a ~a~n"
          (pp/term t1)
          (pp/term t2)))

(define (pp/term t)
  (format "~a" t))

(define (pp/relate i t d)
  (format "~a~n" (cdr d)))

(define (pp/goal i g)
  (match g
    ((disj g1 g2) (format "~a~a" (spaces i) (pp/disj i g1 g2)))
    ((conj g1 g2) (format "~a~a" (spaces i) (pp/conj i g1 g2)))
    ((== t1 t2) "")
    ((relate t d) (format "~a* ~a" (spaces i) (pp/relate i t d)))))

(define (pp/pause i st g)
  (format "pause~n~aState:~n~a~a"
          (spaces (indent i))
          (pp/state (indent (indent i)) st)
          (pp/goal (indent i) g)))

(define (pp/state i st)
  (let ((sub (state-sub st)))
    ;; Each element in `sub` is a pair of a (struct with name and index . value)
    ;; Print each as "(var <name> <index) == value"
    (apply string-append
      (map
        (lambda (binding)
          (format "~a* ~a == ~a~n"
                  (spaces i)
                  (pp/term (car binding))
                  (pp/term (cdr binding))))
        sub))))

(define (pp/result i st)
  (format "~aCandidate result: ~a~n" (spaces i) (reify/initial-var st)))

(define (pp/stream i s)
  (match s
    ((mplus s1 s2) (format "~a~a" (spaces i) (pp/mplus i s1 s2)))
    ((bind s g) (format "~a~a" (spaces i) (pp/bind i s g)))
    ((pause st g) (format "~a~a" (spaces i) (pp/pause i st g)))
    ;; TODO: Review the comment and clause below.
    ;; A state isn't necessarily a result.
    ;; But when a stream is a pair of a state and a stream, then *that* state *is* a result.
    (`(,st . ,s) (format "~a(~n~a~a.~n~a~a)~n"
                   (spaces i)
                   (pp/result (indent i) st)
                   (make-string (indent i) #\space)
                   (pp/stream (indent i) s)
                   (spaces i)))
    ;; I'm hacking into pp/stream to print results. Kind of like stream->choices.
    ((state s diseq types distypbes) (pp/result (indent i) state))
    (s (format "~a~a~n" (spaces i) s))))
