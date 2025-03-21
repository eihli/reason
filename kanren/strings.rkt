#lang racket

(require
 first-order-miniKanren/mk-syntax
 first-order-miniKanren/tools
 first-order-miniKanren/math)

;; Next TODO: https://claude.ai/chat/8e4dcf40-20ce-4674-91f3-e5cbcf229e65
;; Implement if? Let it find recursion?
;; Or... implement more string primitives, like `map` and `nth`.

(define-relation (lowero u l)
  (conde
   ((== u 'e) (== l 'e))
   ((== u 't) (== l 't))
   ((== u 'E) (== l 'e))
   ((== u 'T) (== l 't))))

(define (mapper fn)
  (lambda (xs)
    (cond
      ((null? xs) '())
      (else (cons (fn (car xs)) ((mapper fn) (cdr xs)))))))

((mapper (lambda (x) (+ x 1))) '(1 2 3))
; '(2 3 4)

(define-relation (charo c)
  (conde
   ((== c 'e))
   ((== c 't))
   ((== c 'E))
   ((== c 'T))))

(define (dec x) (- x 1))

(define (nth lst i)
  (cond
    ((null? lst) '())
    ((zero? i) (car lst))
    (else (nth (cdr lst) (dec i)))))

(define-relation (caro a lst val)
  (== val `(,a . ,lst)))

(define-relation (ntho lst idx val)
  (conde
   ((== lst '()) (== val '()))
   ((fresh (cdr-val)
           (zeroo idx) (caro val cdr-val lst)))
   ((fresh (car-lst cdr-lst dec-i)
           (caro car-lst cdr-lst lst)
           (minuso idx '(1) dec-i)
           (ntho cdr-lst dec-i val)))))

(run 20 (q) (ntho q '(0 1) 'E))

(run 1 (q) (ntho '() '(0) q))

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
   ((fresh (c vc)
           (== `(lower ,c) expr)
           (eval-expo c env vc)
           (lowero vc value)))
   ((charo expr)
    (== expr value))
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
   ((fresh (lst idx)
           (== `(nth ,lst ,idx) expr)
           (ntho lst idx value)))
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

(run 20 (q) (eval-expo q '() 'E))

;; (run 1 (q)
;;      (eval-expo `(app ,q E) '() 'e)
;;      (eval-expo `(app ,q T) '() 't))
; '(((lambda (lower (var ())))))

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

(define-syntax define-charo
  (syntax-rules ()
    [(_ (rel-name param) (char-list ...))
     (define-relation (rel-name param)
       (fresh (c)
         (conde
          [(== param 'char-list)] ...)))]))

;; Usage:
;; (define-charo (charo v) (a b c d e f g h i j k l m n o p q r s t u v w x y z
;;                          A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
;;                          | |
;;                          0 1 2 3 4 5 6 7 8 9))

(define-charo (upper-charo v) (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                               | |
                               0 1 2 3 4 5 6 7 8 9))

(define-relation (charso q)
  (conde
   ((fresh (c)
      (charo c)
      (== q `(,c))))
   ((fresh (c cs)
      (charo c)
      (== q `(,c . ,cs))
      (charso cs)))))

(define-relation (stringo s)
  (fresh (c)
    (== s `(string . ,c))
    (charso c)))

(define (make-string s)
  `(string . ,(map string->symbol (map string (string->list s)))))

(make-string "Eric Ihli")

(run 20 (q) (stringo q))

(define-relation (char-to-uppero a b)
  (conde
    ((== a 'a) (== b 'A))
    ((== a 'b) (== b 'B))
    ((== a 'c) (== b 'C))
    ((== a 'd) (== b 'D))
    ((== a 'e) (== b 'E))
    ((== a 'f) (== b 'F))
    ((== a 'g) (== b 'G))
    ((== a 'h) (== b 'H))
    ((== a 'i) (== b 'I))
    ((== a 'j) (== b 'J))
    ((== a 'k) (== b 'K))
    ((== a 'l) (== b 'L))
    ((== a 'm) (== b 'M))
    ((== a 'n) (== b 'N))
    ((== a 'o) (== b 'O))
    ((== a 'p) (== b 'P))
    ((== a 'q) (== b 'Q))
    ((== a 'r) (== b 'R))
    ((== a 's) (== b 'S))
    ((== a 't) (== b 'T))
    ((== a 'u) (== b 'U))
    ((== a 'v) (== b 'V))
    ((== a 'w) (== b 'W))
    ((== a 'x) (== b 'X))
    ((== a 'y) (== b 'Y))
    ((== a 'z) (== b 'Z))
    ((== a 'A) (== b 'A))
    ((== a 'B) (== b 'B))
    ((== a 'C) (== b 'C))
    ((== a 'D) (== b 'D))
    ((== a 'E) (== b 'E))
    ((== a 'F) (== b 'F))
    ((== a 'G) (== b 'G))
    ((== a 'H) (== b 'H))
    ((== a 'I) (== b 'I))
    ((== a 'J) (== b 'J))
    ((== a 'K) (== b 'K))
    ((== a 'L) (== b 'L))
    ((== a 'M) (== b 'M))
    ((== a 'N) (== b 'N))
    ((== a 'O) (== b 'O))
    ((== a 'P) (== b 'P))
    ((== a 'Q) (== b 'Q))
    ((== a 'R) (== b 'R))
    ((== a 'S) (== b 'S))
    ((== a 'T) (== b 'T))
    ((== a 'U) (== b 'U))
    ((== a 'V) (== b 'V))
    ((== a 'W) (== b 'W))
    ((== a 'X) (== b 'X))
    ((== a 'Y) (== b 'Y))
    ((== a 'Z) (== b 'Z))
    ((== a '0) (== b '0))
    ((== a '1) (== b '1))
    ((== a '2) (== b '2))
    ((== a '3) (== b '3))
    ((== a '4) (== b '4))
    ((== a '5) (== b '5))
    ((== a '6) (== b '6))
    ((== a '7) (== b '7))
    ((== a '8) (== b '8))
    ((== a '9) (== b '9))))

(define-relation (char-to-lowero a b)
  (conde
    ((== a 'a) (== b 'a))
    ((== a 'b) (== b 'b))
    ((== a 'c) (== b 'c))
    ((== a 'd) (== b 'd))
    ((== a 'e) (== b 'e))
    ((== a 'f) (== b 'f))
    ((== a 'g) (== b 'g))
    ((== a 'h) (== b 'h))
    ((== a 'i) (== b 'i))
    ((== a 'j) (== b 'j))
    ((== a 'k) (== b 'k))
    ((== a 'l) (== b 'l))
    ((== a 'm) (== b 'm))
    ((== a 'n) (== b 'n))
    ((== a 'o) (== b 'o))
    ((== a 'p) (== b 'p))
    ((== a 'q) (== b 'q))
    ((== a 'r) (== b 'r))
    ((== a 's) (== b 's))
    ((== a 't) (== b 't))
    ((== a 'u) (== b 'u))
    ((== a 'v) (== b 'v))
    ((== a 'w) (== b 'w))
    ((== a 'x) (== b 'x))
    ((== a 'y) (== b 'y))
    ((== a 'z) (== b 'z))
    ((== a 'A) (== b 'a))
    ((== a 'B) (== b 'b))
    ((== a 'C) (== b 'c))
    ((== a 'D) (== b 'd))
    ((== a 'E) (== b 'e))
    ((== a 'F) (== b 'f))
    ((== a 'G) (== b 'g))
    ((== a 'H) (== b 'h))
    ((== a 'I) (== b 'i))
    ((== a 'J) (== b 'j))
    ((== a 'K) (== b 'k))
    ((== a 'L) (== b 'l))
    ((== a 'M) (== b 'm))
    ((== a 'N) (== b 'n))
    ((== a 'O) (== b 'o))
    ((== a 'P) (== b 'p))
    ((== a 'Q) (== b 'q))
    ((== a 'R) (== b 'r))
    ((== a 'S) (== b 's))
    ((== a 'T) (== b 't))
    ((== a 'U) (== b 'u))
    ((== a 'V) (== b 'v))
    ((== a 'W) (== b 'w))
    ((== a 'X) (== b 'x))
    ((== a 'Y) (== b 'y))
    ((== a 'Z) (== b 'z))
    ((== a '0) (== b '0))
    ((== a '1) (== b '1))
    ((== a '2) (== b '2))
    ((== a '3) (== b '3))
    ((== a '4) (== b '4))
    ((== a '5) (== b '5))
    ((== a '6) (== b '6))
    ((== a '7) (== b '7))
    ((== a '8) (== b '8))
    ((== a '9) (== b '9))))

(run 5 (a b) (char-to-uppero 'c b))


(define-relation (chars-to-uppero lower upper)
  (fresh (lower-car lower-cdr upper-car upper-cdr)
         (conde
          ((== lower `())
           (== upper `()))
          ((== lower `(,lower-car . ,lower-cdr))
           (== upper `(,upper-car . ,upper-cdr))
           (char-to-uppero lower-car upper-car)
           (chars-to-uppero lower-cdr upper-cdr)))))

(define-relation (chars-to-lowero upper lower)
  (fresh (lower-car lower-cdr upper-car upper-cdr)
         (conde
          ((== lower `())
           (== upper `()))
          ((== lower `(,lower-car . ,lower-cdr))
           (== upper `(,upper-car . ,upper-cdr))
           (char-to-lowero upper-car lower-car)
           (chars-to-lowero upper-cdr lower-cdr)))))

(run 1 (upper) (chars-to-uppero `(a b c 1 2 3 D E g) upper))

(run 1 (lower) (chars-to-lowero `(A B c 1 2 3 D E g) lower))

(run 1 (lower) (chars-to-lowero `(A B c 1 2 3 D E g) lower))

(make-string "Eric")

;; (run 5 (a b) (eval-expo `(chars-to-lowero ,a ,b) `(,(make-string "Eric")) (make-string "eric"))
;;              )

;; (run 1 (q) (eval-expo `(app ,q) `(,(make-string "Eric")) (make-string "eric"))
;;            (eval-expo `(app ,q) `(,(make-string "Tay")) (make-string "tay")))

;; (run 1 (q) (eval-expo `(chars-to-lowero (A) ,q) '() `(a)))
