#lang racket
(require "tools.rkt")
(provide
 conso
 appendo
 poso
 minuso
 zeroo
 gen-addero
 <lo
 <o
 <=o
 pluso
 eval-expo
 build-num
 make-num
 unbuild-num
 from-zero-countero
 from-one-countero
 *o
 lookupo)

(define-relation (conso head tail result)
   (== `(,head . ,tail) result))

(define-relation (appendo xs ys xsys)
  (conde ((== xs '()) (== ys xsys))
         ((fresh (x zs zsys)
            (== `(,x . ,zs)   xs)
            (== `(,x . ,zsys) xsys)
            (appendo zs ys zsys)))))

(define-relation (nevero x) (nevero x))

(define-relation (alwayso x)
  (conde ((== #t x))
         ((alwayso x))))

(define-relation (sometimeso x)
  (conde ((nevero x))
         ((alwayso x))))

(define-relation (color c)
  (conde ((== c 'red))
         ((== c 'green))
         ((== c 'blue))
         ((== c 'cyan))
         ((== c 'magenta))
         ((== c 'yellow))
         ((== c 'black))
         ((== c 'white))))

(define-relation (shape s)
  (conde ((== s 'circle))
         ((== s 'triangle))
         ((== s 'rectangle))
         ((== s 'pentagon))
         ((== s 'hexagon))))

(define-relation (shape-or-color sc)
  (conde ((shape sc)) ((color sc))))

(define-relation (atomo v)
  (conde
   ((== #t v))
   ((== #f v))
   ((== 'a v))
   ((== 'b v))
   ((== 'c v))
   ((== '0 v))
   ((== '1 v))))

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
    ((fresh (n m nv mv vv)
       (== `(* ,n ,m) expr)
       (eval-expo n env `(number . ,nv))
       (eval-expo m env `(number . ,mv))
       (== value `(number . ,vv))
       (*o nv mv vv)))
    ((fresh (bits)
       (== `(number . ,bits) expr)
       (== expr value)
       (from-zero-countero bits)))
    ((fresh (index)
       (== `(var ,index) expr)        ;; expr is a variable
       (lookupo index env value)))
    ((fresh (body)
       (== `(lambda ,body) expr)      ;; expr is a procedure definition
       (== `(closure ,body ,env) value)))
    ((fresh (rator rand arg env^ body)
            (== `(app ,rator ,rand) expr)  ;; expr is a procedure application
            (eval-expo rator env `(closure ,body ,env^))
            (eval-expo rand env arg)
            (eval-expo body `(,arg . ,env^) value)))
    ;; If this is before lambda, quoted closures become likely.
    ((== `(quote ,value) expr) ;; expr is a literal constant
     (atomo value))
    ((fresh (a*)
       (== `(list . ,a*) expr)        ;; expr is a list operation
       (eval-listo a* env value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)        ;; expr is a cons operation
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (c va vd)
       (== `(car ,c) expr)            ;; expr is a car operation
       (== va value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (c va vd)
       (== `(cdr ,c) expr)            ;; expr is a cdr operation
       (== vd value)
       (eval-expo c env `(,va . ,vd))))
    ))

;; Lookup the value a variable is bound to.
;; Variables are represented namelessly using relative De Bruijn indices.
;; These indices are encoded as peano numerals: (), (s), (s s), etc.
(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i* a d)
         (== `(s . ,i*) index)
         (== `(,a . ,d) e*)
         (lookupo i* e* value))))))

;; This helper evaluates arguments to a list construction.
(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))

(define (evalo expr value) (eval-expo expr '() value))
;; Relational arithmetic
(define (build-num n)
  (cond
    ((odd? n)
     (cons 1
           (build-num (quotient (- n 1) 2))))
    ((and (not (zero? n)) (even? n))
     (cons 0
           (build-num (quotient n 2))))
    ((zero? n) '())))

(define (make-num x)
  `(number . ,(build-num x)))

(define (unbuild-num n)
  (let loop ((n n) (r 0) (i 0))
    (cond
     ((null? n) r)
     ((= 1 (car n)) (loop (cdr n) (+ r (expt 2 i)) (add1 i)))
     (else (loop (cdr n) r (add1 i))))))

(define-relation (from-one-countero q)
  (conde
   ((== q '(1)))
   ((fresh (a d)
           (== q `(,a . ,d))
           (disj* (== a 1) (== a 0))
           (from-one-countero d)))))

(define-relation (from-zero-countero q)
  (conde
   ((== q '()))
   ((from-one-countero q))))

(define-relation (zeroo n)
  (== '() n))

(define-relation (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(define-relation (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(define-relation (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(define-relation (addero d n m r)
  (conde
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r)
              (poso m))
    ((== 1 d) (== '() m)
              (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m)
              (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
                 (fresh (a c)
                   (== `(,a ,c) r)
                   (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r)
                 (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))))

(define-relation (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero d a b c e)
    (addero e x y z)))

(define-relation (pluso n m k)
  (addero 0 n m k))

(define-relation (minuso n m k)
  (pluso m k n))

(define-relation (*o n m p)
  (conde
    ((== '() n) (== '() p))
    ((poso n) (== '() m) (== '() p))
    ((== '(1) n) (poso m) (== m p))
    ((>1o n) (== '(1) m) (== n p))
    ((fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p)))))

(define-relation (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(define-relation (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== `(,a0 . ,x) q)
       (== `(,a1 . ,y) p)
       (conde
         ((== '() n)
          (== `(,a2 . ,z) m)
          (bound-*o x y z '()))
         ((== `(,a3 . ,z) n)
          (bound-*o x y z m)))))))

(define-relation (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(define-relation (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(define-relation (<=lo n m)
  (conde
    ((=lo n m))
    ((<lo n m))))

(define-relation (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(define-relation (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

(define-relation (/o n m q r)
  (conde
    ((== r n) (== '() q) (<o n m))
    ((== '(1) q) (=lo n m) (pluso r m n)
                 (<o r m))
    ((<lo m n)
     (<o r m)
     (poso q)
     (fresh (nh nl qh ql qlm qlmr rr rh)
       (splito n r nl nh)
       (splito q r ql qh)
       (conde
         ((== '() nh)
          (== '() qh)
          (minuso nl r qlm)
          (*o ql m qlm))
         ((poso nh)
          (*o ql m qlm)
          (pluso qlm r qlmr)
          (minuso qlmr nl rr)
          (splito rr r '() rh)
          (/o nh m qh rh)))))))

(define-relation (splito n r l h)
  (conde
    ((== '() n) (== '() h) (== '() l))
    ((fresh (b n^)
       (== `(0 ,b . ,n^) n)
       (== '() r)
       (== `(,b . ,n^) h)
       (== '() l)))
    ((fresh (n^)
       (== `(1 . ,n^) n)
       (== '() r)
       (== n^ h)
       (== '(1) l)))
    ((fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== '() l)
       (splito `(,b . ,n^) r^ '() h)))
    ((fresh (n^ a r^)
       (== `(1 . ,n^) n)
       (== `(,a . ,r^) r)
       (== '(1) l)
       (splito n^ r^ '() h)))
    ((fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== `(,b . ,l^) l)
       (poso l^)
       (splito n^ r^ l^ h)))))

(define-relation (logo n b q r)
  (conde
    ((== '(1) n) (poso b) (== '() q) (== '() r))
    ((== '() q) (<o n b) (pluso r '(1) n))
    ((== '(1) q) (>1o b) (=lo n b) (pluso r b n))
    ((== '(1) b) (poso q) (pluso r '(1) n))
    ((== '() b) (poso q) (== r n))
    ((== '(0 1) b)
     (fresh (a ad dd)
       (poso dd)
       (== `(,a ,ad . ,dd) n)
       (exp2 n '() q)
       (fresh (s)
         (splito n dd r s))))
    ((fresh (a ad add ddd)
       (conde
         ((== '(1 1) b))
         ((== `(,a ,ad ,add . ,ddd) b))))
     (<lo b n)
     (fresh (bw1 bw nw nw1 ql1 ql s)
       (exp2 b '() bw1)
       (pluso bw1 '(1) bw)
       (<lo q n)
       (fresh (q1 bwq1)
         (pluso q '(1) q1)
         (*o bw q1 bwq1)
         (<o nw1 bwq1))
       (exp2 n '() nw1)
       (pluso nw1 '(1) nw)
       (/o nw bw ql1 s)
       (pluso ql '(1) ql1)
       (<=lo ql q)
       (fresh (bql qh s qdh qd)
         (repeated-mul b ql bql)
         (/o nw bw1 qh s)
         (pluso ql qdh qh)
         (pluso ql qd q)
         (<=o qd qdh)
         (fresh (bqd bq1 bq)
           (repeated-mul b qd bqd)
           (*o bql bqd bq)
           (*o b bq bq1)
           (pluso bq r n)
           (<o n bq1)))))))

(define-relation (exp2 n b q)
  (conde
    ((== '(1) n) (== '() q))
    ((>1o n) (== '(1) q)
             (fresh (s)
               (splito n b s '(1))))
    ((fresh (q1 b2)
       (== `(0 . ,q1) q)
       (poso q1)
       (<lo b n)
       (appendo b `(1 . ,b) b2)
       (exp2 n b2 q1)))
    ((fresh (q1 nh b2 s)
       (== `(1 . ,q1) q)
       (poso q1)
       (poso nh)
       (splito n b s nh)
       (appendo b `(1 . ,b) b2)
       (exp2 nh b2 q1)))))

(define-relation (repeated-mul n q nq)
  (conde
    ((poso n) (== '() q) (== '(1) nq))
    ((== '(1) q) (== n nq))
    ((>1o q)
     (fresh (q1 nq1)
       (pluso q1 '(1) q)
       (repeated-mul n q1 nq1)
       (*o nq1 n nq)))))

(define-relation (expo b q n)
  (logo n b q '()))
