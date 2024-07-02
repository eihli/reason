;; Neural Guided Constraint Logic Programming for Neural Architecture Search
;;
;; If you're new to relational programming or kanren and want a video overview
;; (the code demo starts at about 12:40):
;;
;; - https://iv.melmac.space/watch?v=RVDCRlW1f1Y
;;
;; If you want to follow along and have a hands-on exploration of the code:
;;
;; - Install Chez Scheme
;;   - disj
;;     - https://www.scheme.com/download/
;;     - https://formulae.brew.sh/formula/chezscheme
;; - Install a Scheme plugin for your editor
;;   - disj
;;     - VSCode: https://marketplace.visualstudio.com/items?itemName=release-candidate.vscode-scheme-repl
;;     - Emacs: https://www.travishinkelman.com/getting-started-with-chez-scheme-and-emacs/
;;     - Vim: https://github.com/sillybun/vim-repl

;; This file will build a version of a relational programming language known as
;; microKanren, as detailed in this paper:
;; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
;;
;; There is a note in the second-to-last paragraph of that paper
;;
;;   We hope to implement other search strategies in a similarly straightforwrad
;;   manner, and perhaps develop a mechanism to change out search strategies
;;   during the course of a search.
;;
;; That is what we're going to do. We're going to extend microKanren to include
;; a mechanism to change out search strategies during the course of a search.
;;
;; The end goal will be to have a kanren implementation with which we can use a
;; neural network to guide the search, as done in this paper:
;; https://github.com/xuexue/neuralkanren

;;;; First-Order miniKanren
;;
;;
;; De-functionalizing the higher-order representation of goals and streams to a
;; first-order implementation of microKanren
;;
;; What do we need to de-functionalize?
;;
;; The goal constructors, disj, conj, ==, and relate.
;;
;; The stream constructors: stream-append-map, stream-append, and zzz (a.k.a
;; pause in http://minikanren.org/workshop/2019/minikanren19-final2.pdf)
;;
;; In FOMR (First-Order miniKanren), this is done by defining the above in terms
;; of structs, so that they can be decomposed with the help of pattern matching.
;; (Mature streams are represented the same way as higher-order microKanren, a
;; pair of an answer and a remaining stream.)

;; Common code

;; § 1
;;
;;   In a higher-order implementation, goals and streams are represented using
;;   procedures. Procedures encapsulate computational behavior, defining the
;;   search strategy for satisfying constraints. Unfortunately, procedures can't
;;   be modified or decomposed... Our proposal is to use a first-order program
;;   representation, where goals and streams are data structures not coupled to
;;   any search strategy. Since Chez doesn't give us structs...

;; Throughout this, I'll want to be able to write some code here that I can
;; evaluate from my editor but that doesn't get evaluated when this file gets
;; imported or run as a script. Here's a little helper macro to support that.
(define-syntax comment
  (syntax-rules ()
    ((_ . body)
     (begin))))

(define-structure (var name index))

(define (var=? x1 x2)
  (eqv? (var-index x1) (var-index x2)))

;; § 4.1.2 States
;;
;;   States containing constraints are defined using `struct' and track equality
;;   information using a substitution. Other kinds of constraints could be
;;   supported, but for simplicity we will only consider equality constraints in
;;   this implementation.
;;                       ↓ "substitution"
;;                         A substitution is one type of constraint. It's an equality constraint.
;;                         It's possible to modify microKanren to handle other types of constraints.
(define-structure (state sub var-id))
(define empty-sub '())
(define empty-state (make-state empty-sub 0))

;; Search for the final/terminal value of a var in a substitution.
;;
;; Search for a term in a substitution. If you find something, recursively walk
;; the `cdr' of it until you try to walk something that either doesn't exist in
;; the substitution or is not a var.
(define (walk term sub)
  (let ((found (and (var? term)
                    (assp (lambda (x) (var=? x term)) sub))))
    (if found
        (walk (cdr found) sub)
        term)))

(comment
 (let ((x (make-var 'x 0))
       (y (make-var 'y 1)))
   (list
    (walk x `((,x . 1)))
    (walk x `((,x . ,y) (,y . 2)))))
 ;; (1 2)
 )

;; Does x occur in v given the substitution?
;; x might be the fresh variable `x'
;; v might be `y' which unifies to `(z . (x . 1))'
;; In this case, x occurs in v.
(define (occurs? x v sub)
  (let ((x (walk x sub))
        (v (walk v sub)))
    (cond
     ((var? v) (eqv? x v))
     ((pair? v)
      (or (occurs? x (car v) sub)
          (occurs? x (cdr v) sub)))
     (else #f))))

(comment
 (let ((x (make-var 'x 0))
       (y (make-var 'y 1)))
   (let ((sub-no `((,y . 1337)))
         (sub-yes `((,y . (,x . 42)))))
     (list
      (occurs? x y sub-no)
      (occurs? x y sub-yes))))
 ;; (#f #t)
 )

(define (extend-sub v x sub)
  `((,v . ,x) . ,sub))

(comment
 (let ((x (make-var 'x 0)))
   (let ((y (make-var 'y 1))
         (sub `((,x . 1337))))
     (extend-sub y x sub)))
 ;; ((#(var y 1) . #(var x 0))
 ;;  (#(var x 0) . 1337))
 )

;; § 2 The µKanren Language
;;
;;   A µKanren program proceeds through the application of a /goal/ to a
;;   /state/. Goals are often understood by analogy to predicates. Whereas the
;;   application of a predicate to an element of its domain can be either true
;;   or false, a goal pursued in a given state can either succeed or fail.
;;
;;   A goal's success may result in a sequence of (enlarged) states, which we
;;   term a /stream/.
;;
;;   A collection of goals may be satisfied by zero or more states. The result
;;   of a µKanren program is a stream of satisfying states. The stream may be
;;   finite or infinite, as there may be finitely or infinitely many satisfying
;;   states.

;; The first of the four basic goal constructors that we'll look at is the
;; equality goal.
;;
;; The equality goal constructor takes two terms as arguments and returns a goal
;; that succeeds if those two terms /unify/ in the given state. If they unify, a
;; substitution, possibly extended, is returned.
(define (== u v)
  (lambda (state)
    (let ((sub (unify u v (state-sub state))))
      (if sub
          (unit (make-state sub (state-var-id state)))
          mzero))))

;; A goal must return a /stream/ of states. `unit' turns a state into a stream of one element.
(define (unit state) (cons state mzero))

;; `m' == "stream"
;; When you see the `m' prefix, e.g. `mplus', think "stream-plus".
;; This is the empty stream.
(define mzero '())

;; What does it mean to "unify"?
;;
;; - If two terms walk to the same variable, the original substitution is
;; unchanged.
;;
;; - When one term walks to a variable, the substitution is extended, binding
;; the variable to that which the other term walks.
;;
;; - When both terms walk to pairs, the cars and cdrs are unified recursively,
;; succeeding if unification succeeds in both.
;;
;; - When non-variable non-pair terms unify, unification succeeds if they are
;; `eqv?'.
(define (unify u v sub)
  (let ((u (walk u sub))
        (v (walk v sub)))
    (cond
     ((and (var? u) (var? v))
      (var=? u v))
     ((var? u) (extend-sub u v sub))
     ((var? v) (extend-sub v u sub))
     ((and (pair? u) (pair? v))
      (unify (cdr u) (cdr v) (unify (car u) (car v) sub)))
     (else (if (eqv? u v) sub empty-sub)))))

(comment
 (let ((x (make-var 'x 0))
       (y (make-var 'y 1))
       (state empty-state))
   (let ((goal (== x 1)))
     (goal state)))
 ;; (#(state ((#(var x 0) . 1)) 0))
 (let ((x (make-var 'x 0))
       (y (make-var 'y 1))
       (state empty-state))
   (let ((goal (== x 1)))
     (apply
      append
      (map
       (== x y)
       (goal state)))))
 ;; (#(state ((#(var y 1) . 1) (#(var x 0) . 1)) 0))
 )

;; Let's make it so that we don't have to keep calling `(make-var ...)'
;; `call/fresh' is a goal, but it's more like a "goal wrapper" than a goal like
;; `=='. It takes a function whose body is a goal, the function takes a single
;; fresh variable and its body gets run with the updated state.
(define (call/fresh name f)
  (lambda (state)
    (let ((var-id (state-var-id state)))
      ((f (make-var name var-id)) (make-state (state-sub state) (+ 1 (state-var-id state)))))))

(comment
 ((call/fresh 'x (lambda (x)
                   (lambda (state)
                     ((== x 5) state))))
  empty-state)
 ;; (#(state ((#(var x 0) . 5)) 1))
 )

(define (disj g1 g2)
  (lambda (state)
    (mplus (g1 state) (g2 state))))

(define (conj g1 g2)
  (lambda (state)
    (bind (g1 state) g2)))

;; Non-interleaving search
(define (mplus stream-1 stream-2)
  (cond
   ((null? stream-1) stream-2)
   ;;                                       ↓ note the order of stream-1 and stream-2 compared to interleaving search
   ((procedure? stream-1) (lambda () (mplus (stream-1) stream-2)))
   (else (cons (car stream-1) (mplus (cdr stream-1) stream-2)))))

(define (bind stream goal)
  (cond
   ((null? stream) empty-stream)
   ((procedure? stream) (lambda () (bind (stream) goal)))
   (else (mplus (goal (car stream)) (bind (cdr stream) goal)))))

;; Interleaving search
(define (mplus stream-1 stream-2)
  (cond
   ((null? stream-1) stream-2)
   ;;                                       ↓ stream-1 and stream-2 reversed compared to non-interleaving search
   ((procedure? stream-1) (lambda () (mplus stream-2 (stream-1))))
   (else (cons (car stream-1) (mplus (cdr stream-1) stream-2)))))

(define (bind stream goal)
  (cond
   ((null? stream) empty-stream)
   ((procedure? stream) (lambda () (bind (stream) goal)))
   (else (mplus (goal (car stream)) (bind (cdr stream) goal)))))

(define (fives x)
  (disj
   (== x 5)
   (lambda (state) (lambda () ((fives x) state)))))

(define (sixes x)
  (disj
   (== x 6)
   (lambda (state) (lambda () ((sixes x) state)))))

(define (fives-and-sixes x)
  (disj
   (fives x)
   (sixes x)))

(comment
 (let loop ((i 0)
            (stream ((call/fresh 'x fives-and-sixes) empty-state))
            (result '()))
   (if (> i 5)
       result
       (loop
        (+ 1 i)
        ((cdr stream))
        (cons (car stream) result))))
 ;; (#(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1))

 ;; (#(state ((#(var x 0) . 6)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 6)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1)
 ;;  #(state ((#(var x 0) . 6)) 1)
 ;;  #(state ((#(var x 0) . 5)) 1))

 )

(comment
 ((call/fresh 'x
    (lambda (x)
      (call/fresh 'y
        (lambda (y)
          (disj
           (== x 5)
           (== x 10))))))
  empty-state)
 ;; (#(state ((#(var x 0) . 5)) 2) #(state ((#(var x 0) . 10)) 2))

 ((call/fresh 'x
    (lambda (x)
      (call/fresh 'y
        (lambda (y)
          (disj
           (== x 5)
           (conj
            (== x 10)
            (== y 42)))))))
  empty-state)
 ;; (#(state ((#(var x 0) . 5)) 2)
 ;;  #(state ((#(var y 1) . 42) (#(var x 0) . 10)) 2))
 )
