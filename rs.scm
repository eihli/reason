(load "mk.scm")

(run* q
      (== '(((pea)) pod) `(((pea)) ,q)))

(run* q
      (disj2 succeed succeed))

(run* r
  (fresh (x)
    (fresh (y)
      (conj2
       (conj2
        (== 'split x)
        (== 'pea y))
       (== `(,x ,y) r)))))

(expand
 '(run* r
   (fresh (x)
     (fresh (y)
       (conj2
        (conj2
         (== 'split x)
         (== 'pea y))
        (== `(,x ,y) r))))))

(let ((#{r dkb1dri3yl1pice4lgjgk4sej-6} (var (quote r))))
  (($primitive 2 map)
   (reify #{r dkb1dri3yl1pice4lgjgk4sej-6})
   (run-goal #f
     (call/fresh (quote x)
       (lambda (#{x dkb1dri3yl1pice4lgjgk4sej-7})
         (call/fresh (quote y)
           (lambda (#{y dkb1dri3yl1pice4lgjgk4sej-8})
             (conj2
              (conj2
               (== (quote split) #{x dkb1dri3yl1pice4lgjgk4sej-7})
               (== (quote pea) #{y dkb1dri3yl1pice4lgjgk4sej-8}))
              (== (($primitive 2 list) #{x dkb1dri3yl1pice4lgjgk4sej-7} #{y dkb1dri3yl1pice4lgjgk4sej-8}) #{r dkb1dri3yl1pice4lgjgk4sej-6})))))))))

(run* (r x y)
  (conj2
   (conj2
    (== 'split x)
    (== 'pea y))
   (== `(,x ,y) r)))

(expand
 '(run* (r x y)
   (conj2
    (conj2
     (== 'split x)
     (== 'pea y))
    (== `(,x ,y) r))))

(let ((#{q dkb1dri3yl1pice4lgjgk4sej-2} (var (quote q))))
  (($primitive 2 map)
   (reify #{q dkb1dri3yl1pice4lgjgk4sej-2})
   (run-goal #f
     (call/fresh (quote r)
       (lambda (#{r dkb1dri3yl1pice4lgjgk4sej-3})
         (call/fresh (quote x)
           (lambda (#{x dkb1dri3yl1pice4lgjgk4sej-4})
             (call/fresh (quote y)
               (lambda (#{y dkb1dri3yl1pice4lgjgk4sej-5})
                 (conj2
                  (== (($primitive 2 list) #{r dkb1dri3yl1pice4lgjgk4sej-3} #{x dkb1dri3yl1pice4lgjgk4sej-4} #{y dkb1dri3yl1pice4lgjgk4sej-5}) #{q dkb1dri3yl1pice4lgjgk4sej-2})
                  (conj2
                   (conj2
                    (== (quote split) #{x dkb1dri3yl1pice4lgjgk4sej-4})
                    (== (quote pea) #{y dkb1dri3yl1pice4lgjgk4sej-5}))
                   (== (($primitive 2 list) #{x dkb1dri3yl1pice4lgjgk4sej-4} #{y dkb1dri3yl1pice4lgjgk4sej-5})
                       #{r dkb1dri3yl1pice4lgjgk4sej-3}))))))))))))
(run* (q x y)
   (conj2
    (conj2
     (== 'split x)
     (== 'pea y))
    (== `(,x ,y) q)))
