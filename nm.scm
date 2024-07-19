(load "mk.scm")

(defrel (bit-xoro x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

(run* (x y r) (bit-xoro x y 0))
;; ((0 0 _.0) (1 1 _.0))

(defrel (bit-ando x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 0 r))
   ((== 1 x) (== 1 y) (== 1 r))))

(defrel (half-addero x y r c)
  (bit-xoro x y r)
  (bit-ando x y c))

(run 8 (x y r c) (half-addero 1 0 r c))
;; ((_.0 _.1 1 0))

(defrel (full-addero c-in x y r c-out)
  (fresh (r0 c0 c1)
    (half-addero c-in x r0 c0)
    (half-addero r0 y r c1)
    (bit-xoro c0 c1 c-out)))

(run 20 (c-in x y r c-out)
     (full-addero c-in x y r c-out))
;; https://www.build-electronic-circuits.com/full-adder/
;;   i x y r o
;;   ---------
;; ((0 0 0 0 0)
;;  (0 0 1 1 0)
;;  (0 1 0 1 0)
;;  (0 1 1 0 1)
;;  (1 0 0 1 0)
;;  (1 0 1 0 1)
;;  (1 1 0 0 1)
;;  (1 1 1 1 1))

(define (counter v n)
  (disj2
   (== v n)                            ;; First goal, first search branch.
   (disj2                              ;; Second goal, second search branch.
    (== v (add1 n))                    ;; Third goal, third search branch.
    (lambda (s)                        ;; Fourth goal, fourth search branch.
      (lambda ()
        ((counter v (+ 2 n)) s))))))

(run 20 q
     (counter q 0))
;; (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)

(define (counter-1 v n)
  (disj2
   (zzz (== v n))
   (disj2
    (== v (add1 n))
    (zzz (counter-1 v (+ 2 n))))))

(run 20 q (counter-1 q 0))
;; (1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14 17 16 19 18)

(defrel (positive-integer q)
  (counter q 1))

(run 20 q (positive-integer q))

(disj (== 'x 5) (== 'y 9))
;; #<procedure at mk.scm:2027>

(run 1 q (zzz (== q 5)))

(expand '(zzz (== q 5)))


;; (define (guided-counter guide weights inputs v n)
;;   (guide
;;    weights
;;    inputs
;;    (disj-record
;;     (== v n) ;; First goal, first search branch.
;;     (disj
;;      (== v 0)
;;      (lambda (s)  ;; Second goal, second search branch.
;;        (lambda () ;; Third goal, third search branch, jumps back to first goal.
;;          ((counter v (add1 n)) s)))))))

;; (run 20 q
;;      (counter neural-guider weights inputs q 1))
;; (1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0)

(define (disj-record g0 g1)
  (list g0 g1))




(define (build-num n)
  (cond
   ((odd? n) (cons 1 (build-num (/ (sub1 n) 2))))
   ((and (not (zero? n)) (even? n)) (cons 0 (build-num (/ n 2))))
   ((zero? n) '())))

(define (range start . end)
  (if (null? end)
      (range 0 start)
      (let ((end (car end)))
        (if (>= start end)
            '()
            (cons start (range (add1 start) end))))))

(map (lambda (n) (build-num n)) (range 0 8))
;; (()
;;  (1)
;;  (0 1)
;;  (1 1)
;;  (0 0 1)
;;  (1 0 1)
;;  (0 1 1)
;;  (1 1 1))

(defrel (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(run 10 q (poso (build-num 5)))


(define (one? n)
  (= 1 n))

(define (unbuild-num n)
  (let loop ((n n) (r 0) (i 0))
    (cond
     ((null? n) r)
     ((one? (car n)) (loop (cdr n) (+ r (expt 2 i)) (add1 i)))
     (else (loop (cdr n) r (add1 i))))))

(unbuild-num '(0 0 1 1))
(defrel (numbero x)
  (fresh (h t)
    (conj
     (conde
      ((== h 0))
      ((== h 1)))
     (conde
      ((== t '()))
      ((numbero t)))
     (== x `(,h . ,t)))))

(defrel (poso x)
  (fresh (h t)
    (== x `(,h . ,t))))

(defrel (>1o x)
  (fresh (h0 h1 t)
    (== x `(,h0 ,h1 . ,t))))


(run 10 (q x y)
     (>1o q)
     (== q `(0 0)))

(run 10 (x h t)
     (conj
      (conde
       ((== h 1))
       ((== h 0)))
      (== t '())
      (== x `(,h . ,t))))

