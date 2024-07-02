(load "mk.scm")
(load "nm.scm")
(load "numbers.scm")

;; architecture : in_dims layers out_dims
;; layers : layer tail
;; tail : layer | ε
;; layer : flatten | linear
;; flatten : (Flatten in_dim out_dim)
;; linear : (Linear in_dim out_dim)
(defrel (architecture in out)
  (conde
   (fresh (out-1)
     ((linear in out-1)))
   ))

(run 5 (n m p)
  (== p 2)
  (*o n m p))

(run 5 q (== q 5))



(defrel (flatten in out)
  (== out (apply * in)))

(run 5 (a b)
  (== a '(3 5))
  (flatten a `(,b)))

(defrel (some-test in)
  (conde
   (fresh (out)
     (== in out))
   ))

(run 5 x
  (conde
   (fresh (q)
     (== q 1)
     (== q x))))

(define (layer l)
  (conde
    ((== `(linear) l))))

(run 5
  (q)
  (layer q))

(run 5 (q y)
     (conj
      (disj (== y '1)
            (== y #f))
      (conj
       (atomo q)
       (== q y))))

(defrel (linear-layero in-dims out-dims)
  (numbero in-dims)
  (numbero out-dims))

(defrel (fully-connecto layer-1-in layer-1-out layer-2-in layer-2-out)
  (linear-layero layer-1-in layer-1-out)
  (linear-layero layer-2-in layer-2-out)
  (== layer-1-out layer-2-in))

(defrel (linear-layer-fully-connecto out-dims in-dims))
