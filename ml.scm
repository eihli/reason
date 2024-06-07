(load "mk.scm")

(define bit)
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
