#lang racket

(require
 first-order-miniKanren/mk-syntax
 first-order-miniKanren/tools
 first-order-miniKanren/math)

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde
   ((byteo expr) (== expr value))
   ((fresh (n m vn vm)
           (== `(+ ,n ,m) expr)
           (eval-expo n env vn)
           (eval-expo m env vm)
           (byteo vn)
           (byteo vm)
           (byteo value)
           (pluso vn vm value)))
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

(define-relation (byteo b)
  (conde
   ((== b '()))
   ((== b '(1)))
   ((== b '(0 1)))
   ((== b '(1 1)))
   ((== b '(0 0 1)))
   ((== b '(1 0 1)))
   ((== b '(0 1 1)))
   ((== b '(1 1 1)))
   ((== b '(0 0 0 1)))
   ((== b '(1 0 0 1)))
   ((== b '(0 1 0 1)))
   ((== b '(1 1 0 1)))
   ((== b '(0 0 1 1)))
   ((== b '(1 0 1 1)))
   ((== b '(0 1 1 1)))
   ((== b '(1 1 1 1)))
   ((== b '(0 0 0 0 1)))
   ((== b '(1 0 0 0 1)))
   ((== b '(0 1 0 0 1)))
   ((== b '(1 1 0 0 1)))
   ((== b '(0 0 1 0 1)))
   ((== b '(1 0 1 0 1)))
   ((== b '(0 1 1 0 1)))
   ((== b '(1 1 1 0 1)))
   ((== b '(0 0 0 1 1)))
   ((== b '(1 0 0 1 1)))
   ((== b '(0 1 0 1 1)))
   ((== b '(1 1 0 1 1)))
   ((== b '(0 0 1 1 1)))
   ((== b '(1 0 1 1 1)))
   ((== b '(0 1 1 1 1)))
   ((== b '(1 1 1 1 1)))
   ((== b '(0 0 0 0 0 1)))
   ((== b '(1 0 0 0 0 1)))
   ((== b '(0 1 0 0 0 1)))
   ((== b '(1 1 0 0 0 1)))
   ((== b '(0 0 1 0 0 1)))
   ((== b '(1 0 1 0 0 1)))
   ((== b '(0 1 1 0 0 1)))
   ((== b '(1 1 1 0 0 1)))
   ((== b '(0 0 0 1 0 1)))
   ((== b '(1 0 0 1 0 1)))
   ((== b '(0 1 0 1 0 1)))
   ((== b '(1 1 0 1 0 1)))
   ((== b '(0 0 1 1 0 1)))
   ((== b '(1 0 1 1 0 1)))
   ((== b '(0 1 1 1 0 1)))
   ((== b '(1 1 1 1 0 1)))
   ((== b '(0 0 0 0 1 1)))
   ((== b '(1 0 0 0 1 1)))
   ((== b '(0 1 0 0 1 1)))
   ((== b '(1 1 0 0 1 1)))
   ((== b '(0 0 1 0 1 1)))
   ((== b '(1 0 1 0 1 1)))
   ((== b '(0 1 1 0 1 1)))
   ((== b '(1 1 1 0 1 1)))
   ((== b '(0 0 0 1 1 1)))
   ((== b '(1 0 0 1 1 1)))
   ((== b '(0 1 0 1 1 1)))
   ((== b '(1 1 0 1 1 1)))
   ((== b '(0 0 1 1 1 1)))
   ((== b '(1 0 1 1 1 1)))
   ((== b '(0 1 1 1 1 1)))
   ((== b '(1 1 1 1 1 1)))))

(define-relation (caro a d ad)
  (== ad `(,a . ,d)))

(define-relation (byte-arrayo b)
  (conde
   ((== b `()))
   ((fresh (ba bd)
           (caro ba bd b)
           (byteo ba)
           (byte-arrayo bd)))))

(define-relation (charo b c)
  (conde
   ((== '0 c) (== b '()))
   ((== '1 c) (== b '(1)))
   ((== '2 c) (== b '(0 1)))
   ((== '3 c) (== b '(1 1)))
   ((== '4 c) (== b '(0 0 1)))
   ((== '5 c) (== b '(1 0 1)))
   ((== '6 c) (== b '(0 1 1)))
   ((== '7 c) (== b '(1 1 1)))
   ((== '8 c) (== b '(0 0 0 1)))
   ((== '9 c) (== b '(1 0 0 1)))
   ((== 'A c) (== b '(0 1 0 1)))
   ((== 'B c) (== b '(1 1 0 1)))
   ((== 'C c) (== b '(0 0 1 1)))
   ; ...
   ((== 'a c) (== b '(0 0 1 0 1)))
   ((== 'b c) (== b '(1 0 1 0 1)))
   ((== 'c c) (== b '(0 1 1 0 1))))
  ; ...
  )

(define-relation (stringo byte-array str)
  (byte-arrayo byte-array)
  (conde
   ((== byte-array `()) (== str `()))
   ((fresh (ba bd stra strd)
           (caro ba bd byte-array)
           (caro stra strd str)
           (charo ba stra)
           (stringo bd strd)))))

(run 1 (q) (byte-arrayo q))
(run 5 (q) (byte-arrayo q))

(run 1 (b s)
     (fresh (r)
            (== s `(a . ,r))
            (stringo b s)))

(run* (q) (eval-expo `(+ (1 1) (0 1)) '() q))

(run 20 (n m) (eval-expo `(+ ,n ,m) '() '(1 0 1)))
