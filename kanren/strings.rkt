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

; ((mapper (lambda (x) (+ x 1))) '(1 2 3))
; '(2 3 4)

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




;; (run 1 (q)
;;      (eval-expo `(app ,q) '(,(make-string "E")) '((e)))
;;      (eval-expo `(app ,q) '(,(make-string "T")) '((t))))

;; (run 20 (q)
;;      (eval-expo q '(,(make-string "ERIC")) '((e))))

;; (make-string "ERIC")

;; (run 1 (q) (eval-expo `(lower ,(make-string "ERIC")) '() q))
;; (run 1 (q) (eval-expo `(lower ,(make-string "TAY")) '() q))

;; (run 1 (q) (eval-expo q '() `(char e)))

;; (run 1 (q)
;;      (eval-expo `(app ,q ,(make-string "ERIC")) '() '(e r i c))
;;      (eval-expo `(app ,q ,(make-string "TAY")) '() '(t a y))
;;      )

;; (run 1 (q)
;;      (== q `(lambda (list (car (lower (var ()))))))
;;      (eval-expo `(app ,q ,(make-string "ERIC")) '() '(e))
;;      (eval-expo `(app ,q ,(make-string "TAY")) '() '(t))
;;      )

;; (time
;;  (run 1 (q)
;;       (eval-expo `(app ,q ,(make-string "ERIC")) '() '(e))
;;       (eval-expo `(app ,q ,(make-string "TAY")) '() '(t))
;;       ))

;; (time
;;  (run 1 (q r)
;;       (== q `(app (lambda (list (var ()) (var (s)))) (char a)))
;;       (eval-expo q `(b) r)))

(time
 (run 1 (q r)
      (== q `(app
              (lambda
                (list (var ())
                      (app
                       (lambda (var (s)))
                       (char b))))
              (char a)))
      (eval-expo q `() r)))

;; (run 1 (a b c)
;;      (== a `(app (lambda (list (var ()) (var (s)))) (char a)))
;;      (eval-expo a b c)
;;      )

;; (run 20 (a b) (eval-expo `(char ,a) '() b))

;; (run 10 (q) (eval-expo q '() `(e r i c)))

;; TODO:
;; I'm in the middle of refactoring to make "strings" simply "lists of (char <c>)".
;; Think about how you would apply lower to every character in a list.

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
   ((fresh (c vc)
           (== `(lower ,c) expr)          ;; expr is a to-lowercase operation
           (eval-expo c env vc)
           (chars-to-lowero vc value)))
   ((fresh (c vc)
           (== `(upper ,c) expr)          ;; expr is a to-lowercase operation
           (eval-expo c env vc)
           (chars-to-lowero vc value)))
   ((fresh (c)
           (== `(char ,c) expr)           ;; expr is a character
           (charo value)
           (eval-expo c env value)))
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
   ((fresh (bits)                         ;; expr is a number
           (== `(number . ,bits) expr)
           (== expr value)
           (from-zero-countero bits)))
   ))

; (run 20 (q) (eval-expo q '() 'E))

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
          [(== param `char-list)] ...)))]))

;; Usage:
(define-charo (charo v) (a b c d e f g h i j k l m n o p q r s t u v w x y z
                         A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                         | |
                         0 1 2 3 4 5 6 7 8 9))

(run 5 (q) (charo q))

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
    (== s `(list . ,c))
    (charso c)))

(define (make-string s)
  `(list . ,(map (lambda (x) `(char ,(string->symbol x))) (map string (string->list s)))))

(make-string "Eric Ihli")

(run 20 (q) (stringo q))

(define-relation (char-to-uppero a b)
  (conde
   ((== a `(char a)) (== b `(char A)))
   ((== a `(char b)) (== b `(char B)))
   ((== a `(char c)) (== b `(char C)))
   ((== a `(char d)) (== b `(char D)))
   ((== a `(char e)) (== b `(char E)))
   ((== a `(char f)) (== b `(char F)))
   ((== a `(char g)) (== b `(char G)))
   ((== a `(char h)) (== b `(char H)))
   ((== a `(char i)) (== b `(char I)))
   ((== a `(char j)) (== b `(char J)))
   ((== a `(char k)) (== b `(char K)))
   ((== a `(char l)) (== b `(char L)))
   ((== a `(char m)) (== b `(char M)))
   ((== a `(char n)) (== b `(char N)))
   ((== a `(char o)) (== b `(char O)))
   ((== a `(char p)) (== b `(char P)))
   ((== a `(char q)) (== b `(char Q)))
   ((== a `(char r)) (== b `(char R)))
   ((== a `(char s)) (== b `(char S)))
   ((== a `(char t)) (== b `(char T)))
   ((== a `(char u)) (== b `(char U)))
   ((== a `(char v)) (== b `(char V)))
   ((== a `(char w)) (== b `(char W)))
   ((== a `(char x)) (== b `(char X)))
   ((== a `(char y)) (== b `(char Y)))
   ((== a `(char z)) (== b `(char Z)))
   ((== a `(char A)) (== b `(char A)))
   ((== a `(char B)) (== b `(char B)))
   ((== a `(char C)) (== b `(char C)))
   ((== a `(char D)) (== b `(char D)))
   ((== a `(char E)) (== b `(char E)))
   ((== a `(char F)) (== b `(char F)))
   ((== a `(char G)) (== b `(char G)))
   ((== a `(char H)) (== b `(char H)))
   ((== a `(char I)) (== b `(char I)))
   ((== a `(char J)) (== b `(char J)))
   ((== a `(char K)) (== b `(char K)))
   ((== a `(char L)) (== b `(char L)))
   ((== a `(char M)) (== b `(char M)))
   ((== a `(char N)) (== b `(char N)))
   ((== a `(char O)) (== b `(char O)))
   ((== a `(char P)) (== b `(char P)))
   ((== a `(char Q)) (== b `(char Q)))
   ((== a `(char R)) (== b `(char R)))
   ((== a `(char S)) (== b `(char S)))
   ((== a `(char T)) (== b `(char T)))
   ((== a `(char U)) (== b `(char U)))
   ((== a `(char V)) (== b `(char V)))
   ((== a `(char W)) (== b `(char W)))
   ((== a `(char X)) (== b `(char X)))
   ((== a `(char Y)) (== b `(char Y)))
   ((== a `(char Z)) (== b `(char Z)))
   ((== a `(char 0)) (== b `(char 0)))
   ((== a `(char 1)) (== b `(char 1)))
   ((== a `(char 2)) (== b `(char 2)))
   ((== a `(char 3)) (== b `(char 3)))
   ((== a `(char 4)) (== b `(char 4)))
   ((== a `(char 5)) (== b `(char 5)))
   ((== a `(char 6)) (== b `(char 6)))
   ((== a `(char 7)) (== b `(char 7)))
   ((== a `(char 8)) (== b `(char 8)))
   ((== a `(char 9)) (== b `(char 9)))))

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

(run 1 (a b) (chars-to-lowero '(E R I C) b))

(define-relation (string-to-lowero upper lower)
  (fresh (uc lc)
         (== upper `(list . ,uc))
         (== lower `(list . ,lc))
         (chars-to-lowero uc lc)))

(run 1 (q) (string-to-lowero (make-string "AbCd") q))

(make-string "Eric")

(run 1 (q) (eval-expo `(lower ,(make-string "Eric")) '()  q))

;; (run 1 (q)
;;      (eval-expo `(app ,q (var ())) `((,make-string "A")) (make-string "a"))
;;      (eval-expo `(app ,q (var ())) `((,make-string "B")) (make-string "b")))

;; (run 1 (q)
;;      (eval-expo `(app ,q (var ())) `(,(make-string "Eric")) (make-string "e"))
;;      (eval-expo `(app ,q (var ())) `(,(make-string "Tay")) (make-string "t"))
;;      )

;; (run 1 (q) (eval-expo `(chars-to-lowero (A) ,q) '() `(a)))
