;; The output format of this pretty printing is still kind of noisy.
;; It makes it hard to find, by hand, the path you want.
;;
;; Example:
;;
;; left or right? right
;; left:
;; mplus
;;   #s(state ((#s(var b 2) 3 4) (#s(var a2 7)) (#s(var res 8) 3 4) (#s(var a1 6) . 2) (#s(var a2 4) #s(var a1 6) . #s(var a2 7)) (#s(var res 5) 2 3 4) (#s(var a1 3) . 1) (#s(var a 1) #s(var a1 3) . #s(var a2 4)) (#s(var #f 0) #s(var a 1) #s(var b 2))) () () ())
;;   #f
;;   mplus
;;     pause
;;       #s(state ((#s(var b 2) 4) (#s(var a2 10)) (#s(var res 11) 4) (#s(var a1 9) . 3) (#s(var a2 7) #s(var a1 9) . #s(var a2 10)) (#s(var res 8) 3 4) (#s(var a1 6) . 2) (#s(var a2 4) #s(var a1 6) . #s(var a2 7)) (#s(var res 5) 2 3 4) (#s(var a1 3) . 1) (#s(var a 1) #s(var a1 3) . #s(var a2 4)) (#s(var #f 0) #s(var a 1) #s(var b 2))) () () ())
;;       conj
;;         == () ()
;;         == (4) (4)
;;     pause
;;       #s(state ((#s(var res 14)) (#s(var a1 12) . 4) (#s(var a2 10) #s(var a1 12) . #s(var a2 13)) (#s(var res 11) 4) (#s(var a1 9) . 3) (#s(var a2 7) #s(var a1 9) . #s(var a2 10)) (#s(var res 8) 3 4) (#s(var a1 6) . 2) (#s(var a2 4) #s(var a1 6) . #s(var a2 7)) (#s(var res 5) 2 3 4) (#s(var a1 3) . 1) (#s(var a 1) #s(var a1 3) . #s(var a2 4)) (#s(var #f 0) #s(var a 1) #s(var b 2))) () () ())
;;       conj
;;         (#<procedure:appendo> appendo #s(var a2 13) #s(var b 2) ())
;;         == (4 . #s(var a2 13)) (4 . #s(var a2 13))
;;         == (4) (4)
;;
;; right:
;; pause
;;   #s(state ((#s(var b 2) 1 2 3 4) (#s(var a 1)) (#s(var #f 0) #s(var a 1) #s(var b 2))) () () ())
;;   conj
;;     == () ()
;;     == (1 2 3 4) (1 2 3 4)
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
               (out (format "conj~n")))
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
  (format "~a~n" d))

(define (pp/goal i g)
  (match g
    ((disj g1 g2) (format "~a~a" (spaces i) (pp/disj i g1 g2)))
    ((conj g1 g2) (format "~a~a" (spaces i) (pp/conj i g1 g2)))
    ((== t1 t2) (format "~a~a" (spaces i) (pp/== i t1 t2)))
    ((relate t d) (format "~a~a" (spaces i) (pp/relate i t d)))))

(define (pp/pause i st g)
  (format "pause~n~a~a~a"
          (spaces (indent i))
          (pp/state (indent i) st)
          (pp/goal (indent i) g)))

(define (pp/state i st)
  (format "~a~n" st))

(define (pp/stream i s)
  (match s
    ((mplus s1 s2) (format "~a~a" (make-string i #\space) (pp/mplus i s1 s2)))
    ((bind s g) (format "~a~a" (make-string i #\space) (pp/bind i s g)))
    ((pause st g) (format "~a~a" (make-string i #\space) (pp/pause i st g)))
    (`(,st . ,s) (format "~a~a~a" (make-string i #\space) (pp/state i st) (pp/stream i s)))
    (s (format "~a~a~n" (make-string i #\space) s))))

