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

(defrel (linear-layero in-dims out-dims)
  (numbero in-dims)
  (numbero out-dims))

(defrel (fully-connecto layer-1-in layer-1-out layer-2-in layer-2-out)
  (linear-layero layer-1-in layer-1-out)
  (linear-layero layer-2-in layer-2-out)
  (== layer-1-out layer-2-in))

(defrel (linear-layer-fully-connecto out-dims in-dims))
