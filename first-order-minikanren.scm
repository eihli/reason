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

(define (initial-var) (var #f 0))

;; We'll be turning this into a macro later. That's why I'm prefixing this
;; "fresh" with "call/".
(define call/fresh
  (let ((index 0))
    (lambda (name)
      (set! index (+ 1 index))
      (var name index))))

;; § 4.1.2 States
;;
;;   States containing constraints are defined using `struct' and track equality
;;   information using a substitution. Other kinds of constraints could be
;;   supported, but for simplicity we will only consider equality constraints in
;;   this implementation.

(define empty-sub '())

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
 (let ((x (call/fresh 'x))
       (y (call/fresh 'y)))
   (list
    (walk x `((,x . 1)))
    (walk x `((,x . ,y) (,y . 2)))))
 ;; (1 2)
 )

;; Does var x occur in term given the substitution?
;;
;; Why do we need this?
;;
;; Imagine your code tries to relate the var `x' to the term `(y . x)'. That
;; will result in an infinite loop when you try to walk the value of x.
;;
;; That's why we need `occurs?'. Before we extend a substitution, let's make
;; sure there's no infinitely looping relations.
(define (occurs? x term sub)
  (cond
   ((pair? term) (or (occurs? x (walk (car term) sub) sub)
                     (occurs? x (walk (cdr term) sub) sub)))
   ((var? term)  (var=? term x))
   (else         #f)))

(comment
 (let ((x (call/fresh 'x))
       (y (call/fresh 'y))
       (z (call/fresh 'z)))
   (let ((sub `((,x . ,y) (,y . 2))))
     (list
      (occurs? x nksub)
      (occurs? y sub)
      (occurs? z sub))))

 )

(define (extend-sub x term sub)
  (if (not (occurs? x term sub))
      `((,x . ,term) . ,sub)
      sub))

(defrecord state state? state-sub)

(define empty-state (state empty-sub))

(comment
 (let ((some-state empty-state)
       (x (call/fresh 'x))
       (y (call/fresh 'y)))
   (list
    (extend-sub x y (state-sub some-state))
    (extend-sub x y (extend-sub x y (state-sub some-state)))))
 (let ((x (call/fresh 'x))
       (y (call/fresh 'y)))
   (eqv? (var-index x) (var-index x)))

 )

;; § 4.1.3 Streams and Unification
;;
;;   Unification enforces equality between two terms and returns a substitution.
;;   It is defined as `unify'.

;; We'll eventually define a `unify' function that takes a `state' instead of a
;; substitution. That's why we call this one `unify/sub'.
(define (unify/sub u v sub)
  (let ((u (walk u sub))
        (v (walk v sub)))
    ))
