#lang racket

(require
 first-order-miniKanren/mk-syntax
 first-order-miniKanren/tools)

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
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

(define-relation (atomo v)
  (conde
   ((== #t v))
   ((== #f v))
   ((== 'a v))
   ((== 'b v))
   ((== 'c v))
   ((== '0 v))
   ((== '1 v))))

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
(define-charo (charo v) (a b c d e f g h i j k l m n o p q r s t u v w x y z
                         A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                         0 1 2 3 4 5 6 7 8 9))

(define-charo (upper-charo v) (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                               0 1 2 3 4 5 6 7 8 9))


(run* (q) (charo q))

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

(run 1 (upper) (chars-to-uppero `(a b c 1 2 3 D E g) upper))

