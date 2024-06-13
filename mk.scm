(define (var name) (vector name))
(define (var? x) (vector? x))

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))

(define empty-s '())

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond
     ((pair? a) (walk (cdr a) s))
     (else v))))

(walk w `((,w . ,v) (,u . 1) (,v . ,u)))
(assv w `((,w . ,v)))
(walk w `((,w . ,u) (,u . 1)))

;; Occurs and Extend Substitution
;;
;; Extend the substitution s with (x . v) if x
;; is not already in the list and if doing so would not create a cycle.
;; (define (extend-s x v s)
;;   ())

;; x in s and walk v not equal to x
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (eqv? v x))
     ((pair? v)
      (or (occurs? x (car v) s)
          (occurs? x (cdr v) s)))
     (else #f))))

(occurs? w v `((,v . (,w . ,w))))
(occurs? w v `((,v . ,w)))

(define (extend-s x v s)
  (cond
   ((occurs? x v s) #f)
   (else (cons `(,x . ,v) s))))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))
(extend-s x `(,x) empty-s)
(extend-s x `(,y) `((,y . ,x)))
(let ((s `((,z . ,x) (,y . ,z))))
  (let ((s (extend-s x 'e s)))
    (and s (walk y s))))

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    ;; Both are vars and are equal
    ;; One is a var, one is an atom, unify the var to the atom
    ;; One is a pair, if var occurs in the pair, f, else unify
    (cond
     ((eqv? u v) s)
     ((var? u) (extend-s u v s))
     ((var? v) (extend-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else #f))))

(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s `(,s) '()))))

(define succeed
  (lambda (s)
    `(,s)))

(define fail
  (lambda (s)
    '()))

((== #f #f) empty-s)
((== #t #f) empty-s)

(define (append-inf s-inf t-inf)
  (cond
   ((null? s-inf) t-inf)
   ((pair? s-inf) (cons (car s-inf) (append-inf (cdr s-inf) t-inf)))
   (else (lambda () (append-inf t-inf (s-inf))))))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(define (nevero)
  (lambda (s)
    (lambda ()
      ((nevero) s))))

(let ((s-inf ((disj2 (nevero) (== 'olive x)) empty-s)))
  (s-inf))

(let ((s-inf (disj2 (nevero) (== 'olive w))))
  ((s-inf empty-s)))

(define (alwayso)
  (lambda (s)
    (lambda ()
      ((disj2 succeed (alwayso)) s))))

(define (force s)
  (cond
   ((pair? s) s)
   (else (s))))

(define (take-inf n s-inf)
  (cond
   ((and n (zero? n)) '())
   ((null? s-inf) '())
   ((pair? s-inf) (cons (car s-inf) (take-inf (and n (sub1 n)) (cdr s-inf))))
   (else (take-inf n (s-inf)))))


(let ((x (var 'x)))
  (take-inf
   5
   ((disj2
     (disj2 (== 'olive x) (== 'peanut x))
     (== 'oil x)) empty-s)))


;; In other words, append-map-inf tries to satisfy the goal with everything in the stream.
(define (append-map-inf g s-inf)
  (cond
   ((null? s-inf) '())
   ((pair? s-inf)
    (append-inf (g (car s-inf)) (append-map-inf g (cdr s-inf))))
   (else
    (lambda ()
      (append-map-inf g (s-inf))))))

;; What's conj2 going to return?
;; A goal. The goal takes a substitution and returns a stream of substitutions.
;; Therefore, append-map-inf returns a stream of substitutions
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(append-map-inf
 (== x 2)
 `(((,x . 1)) ((,x . 2) (,y . x)) ((,x . ,y))))

(let ((x (var 'x)))
  (take-inf
   5
   ((conj2
     (== 'olive x)
     (== 'oil y))
    empty-s)))

((== 'olive x) empty-s)

;; Takes a name and a function.
;; Returns the function called with the name as a variable.
;; a.k.a returns a goal
(define (call/fresh name f)
  (f (var name)))

(take-inf 1 ((call/fresh 'foo (lambda (variable)
                                (== variable 'olive))) empty-s))

(define (reify-name n)
  (string->symbol
   (string-append "_." (number->string n))))

(reify-name 0)

;; Walk s to find v.
;; For each var in v, replace var with walk s to find var
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v)
      (cons (walk* (car v) s)
            (walk* (cdr v) s)))
     (else v))))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(let ((s `((,x . b) (,z . ,y) (,w . (,x e ,z)))))
  (walk* w s))

;; v is a list that contains variables that haven't necessarily been reified
;; r is an association list of variables to their reified names
`((,x . 3) (,y . ,z))
`((,x . ,y) (,y . ,z))

(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
     ((var? v)
      (cons `(,v . ,(reify-name (length r))) r))
     ((pair? v)
      (reify-s (cdr v) (reify-s (car v) r)))
     (else r))))

(let ((r empty-s))
  (reify-s x r))

;; Now that we know how to create a reified substitution,
;; how should we use the substitution to replace all the
;; fresh variables in a value?

(define (reify v)
  (lambda (s)
    (let ((v (walk* v s)))
      (let ((r (reify-s v empty-s)))
        (walk* v r)))))

(let ((a `(,x . (,y ,z 3 'b ,u)))
      (b `(,y . 1))
      (c `(,z . 'foobar)))
  ((reify v) `(,a ,b ,c)))

;; What is the value of
(map (reify x)
     (take-inf 5
               ((disj2 (== 'olive x) (== 'oil x)) empty-s)))

(define (run-goal n g)
  (take-inf n (g empty-s)))

(map (reify x) (run-goal 5 (disj2 (== 'olive x) fail)))

;; We can now define appendo from 4:41, replacing conde with fresh
;; and defrel with the functions defined above

;; What is l, t, out?
;; They can each be:
;; - var
;; - list
;; What do we do if they are all lists?
;;   Just return a stream with the substitution (append l t out)?
;;

(define (nullo v)
  (lambda (s)
    (lambda ()
      ((== v '()) s))))

;;;; Syntax, macros, etc...

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                     (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
            (run-goal n (conj g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...) (conj g ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x0
                 (lambda (x0)
                   (fresh (x ...) g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(run* q (fresh (x y)  (== `(,x ,y) q)))

;; TODO: When should you use null? vs nullo?
;; Could the below be re-written with nullo?
;; What does null? do?
;;   It returns true or false
;; What does nullo do?
;;   It produces a goal that tries to unify a variable with '()
;; How can you write code (if at all) that would let you replace null? with nullo?
(define (ifte g0 g1 g2)
  (lambda (s)
    (let loop ((s-inf (g0 s)))
      (cond
       ((null? s-inf) (g2 s))
       ((pair? s-inf) (append-map g1 s-inf))
       (else
        (lambda ()
          (loop (s-inf))))))))

(define (once g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond
       ((null? s-inf) '())
       ((pair? s-inf) (cons (car s-inf) '()))
       (else
        (loop (s-inf)))))))

((once (disj2 (== #t x) (== #f x))) empty-s)

(define-syntax conda
  (syntax-rules ()
    ((conda (g0 g ...)) (conj g0 g ...))
    ((conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...)))))

(define-syntax condu
  (syntax-rules ()
    ((condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...))))

(define-syntax zzz
  (syntax-rules ()
    ((zzz g)
     (lambda (s) (lambda () (g s))))))

(expand '(zzz (== q 5)))

;; Define a simple macro
(define-syntax my-macro
  (syntax-rules ()
    ((my-macro x)
     (+ x 1))))

;; Now, let's expand the macro
(expand '(my-macro 10))
