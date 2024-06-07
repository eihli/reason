(load "mk.scm")

(defrel (bit-xoro x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

(run 8 (x y r) (bit-xoro x y 0))

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

(defrel (full-addero c-in x y r c-out)
  (fresh (r0 c0 c1)
    (half-addero c-in x r0 c0)
    (half-addero r0 y r c1)
    (bit-xoro c0 c1 c-out)))

(run 20 (c-in x y r c-out)
     (full-addero c-in x y r c-out))

(define (build-num n)
  (cond
   ((odd? n) (cons 1 (build-num (/ (sub1 n) 2))))
   ((and (not (zero? n)) (even? n)) (cons 0 (build-num (/ n 2))))
   ((zero? n) '())))

(define (one? n)
  (= 1 n))

(define (ngpt n)
  (ceiling))

(define (unbuild-num n)
  (let loop ((n n) (r 0) (i 0))
    (cond
     ((null? n) r)
     ((one? (car n)) (loop (cdr n) (+ r (expt 2 i)) (add1 i)))
     (else (loop (cdr n) r (add1 i))))))

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

(run 3 (adder 0 x y z))

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

(run 10 (x y z)
     (bito x)
     (bito y)
     (bito z))
