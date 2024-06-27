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

;; Let's get started.
;;
;; First, some data structures and nomenclature...
;;
;; "state" holds all of the global data that gets passed around in miniKanren. I
;; say "global", but we're writing this in a functional style, so it's only
;; global in the sense that it gets passed around to every function that needs
;; it. But it's not global in the sense of shared memory.
;;
;; What does the state entail?
;;
;; 1) a stream (≆ a list) of "substitutions"
;; 2) an incrementing counter that servces as an identifier for "fresh" variables
;;
;; What is a "stream of substitutions?
;;
;; The whole point of this relational system is to be able to say something
;; like: "x relates to y in the sense that they are the same thing" and "y
;; relates to 1 in the sense that they are the same thing". This is a trival
;; example that undermines a more significant purpose of relational systems, but
;; it serves to demonstrate what is a stream of substitutions. A "substitution"
;; is a list that tracks the fact that x relates to y and y relates to 1. So,
;; later, if we want to figure out what is the ultimate value of x, we can walk
;; the substitution.
;;
;; We need a "stream" of substitutions because it's possible to create a
;; relation of the form "y is either 0 or 1". For that, we need one substitution
;; where y associates with 0 and another substitution where y associates with 1.
;;
;; Why is a stream "kind of" like a list, but not really a list?
;;
;; Imagine if we had a relation of the form "y is greater than 0". That would be
;; an infinite-length list. If we tried to operate on it, our program would
;; never terminate. A "stream" is a list in the sense of it's a "cons cell" (it
;; has a head and a tail), but the tail might be a function call instead of a
;; list. The function call will resolve to something that has a head and a tail
;; when it's invoked. So, you can always continue walking down the list, at your
;; leisure, by invoking the tail (when the tail is a function). (This isn't a
;; new concept. Nor is it specific to miniKanren. This is basically what the
;; `range` function does in Python, for example. I'm just calling out here to
;; clarify any confusion around the term "stream of substitutions".)
;;
;;                   Substitutions__    __Counter used to create unique fresh vars
;;                                  |  |
(define empty-state (vector 'state '() 0))
;;                                      |__Subsequent locations can be used for more constraints.
;;                                      |__See "Annotated Implementation of miniKanren with Constraints"

(define (state-subst state) (vector-ref state 1))
(define (state-varid state) (vector-ref state 2))
(define (make-state subst varid) (vector 'state subst varid))

(define (var id) (vector id))
(define (var-id x) (vector-ref x 0))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (var-id x1) (var-id x2)))

;; Let's visualize our substitutions
;;
;; A single substition (as part of a stream of them) might look like this:
;;
;; ( (#(0) . #(1)) (#(1) . 1) )
;;
;; You can translate that to the example I used above where I said "x relates to
;; y in the sence that they are the same thing" and "y relates to 1 in the sense
;; that they are the same thing":
;;
;; ( (  x  =   y ) (  y  = 1) )
;;
;; What operation might we want to perform on a substitution?
;;
;; We probably want to be able to answer "what is the ultimate value of x".
;;
;; To do that, we can search the substitution for the first pair that starts
;; with "x", and then if its tail is also a var, then search the substitution
;; for the first pair that starts with its tail. And continue rceursively until
;; we find something that is not a var or is a var but has no association.
(define (walk u subst)
  (let ((pr (and (var? u)
                 (assp
                  (lambda (v)
                    (var=? u v))
                  subst))))
    (if pr
        (walk (cdr pr) subst)
        u)))

;; Now... I want to be able to write some code here that I can evaluate from my
;; editor but that doesn't get evaluated when this file gets imported or run as
;; a script. Here's a little helper macro to support that.
(define-syntax comment
  (syntax-rules ()
    ((_ . body)
     (begin))))

(comment
 (let ((x (var 0))
       (y (var 1))
       (z (var 2)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (list
      (format #f "walked value of x: ~a" (walk x subst))
      (format #f "walked value of y: ~a" (walk y subst))
      (format #f "walked value of z: ~a" (walk z subst))
      (format #f "walked value of 7: ~a" (walk 7 subst)))))
 ;; ("walked value of x: 1337" "walked value of y: 1337" "walked value of z: #(2)" "walked value of 7: 7")
 )


;; And to add a new association (a.k.a "binding") to the substitution...
(define (extend-subst u v subst)
  `((,u . ,v) . ,subst))

(comment
 (let ((u (var 0))
       (v (var 1))
       (x (var 2))
       (subst '()))
   (extend-subst x 1 (extend-subst u v subst)))
 )

;; One thing we'll want to do as part of our relational system is to "unify" two
;; terms. Or, at least, attempt to. Unification may fail, and that's perfectly
;; valid and expected.
;;
;; What is a "term" when talking about "unifying two terms"?
;;
;; A "term" is either a var, a primitive scheme object like a number, or a pair
;; of the foregoing.
;;
;; To unify two terms, walk them both in a substitution. If they walk to the
;; same var, then the original substitution is returned unchanged. If one walks
;; to a var, then the substitution is extended with the variable bound to the
;; term that the other term walked to. If both terms walk to pairs, then the
;; heads and tails of each are unified recursively. And finally, non-variable
;; non-pair terms unify if they are equal. Anything else, unification fails.
(define (unify u v subst)
  (let ((u (walk u subst))
        (v (walk v subst)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (extend-subst u v subst))
     ((var? v) (extend-subst v u subst))
     ((and (pair? u) (pair? v))
      (let ((new-subst (unify (car u) (car v) subst)))
        (if new-subst
            (unify (cdr u) (cdr v) new-subst)
            empty-substitution)))
     (else (if (eqv? u v)
               subst
               empty-substitution)))))

(comment
 (let ((x (var 0))
       (y (var 1))
       (z (var 2)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (let ((before-unify (walk z subst)))
       (let ((new-subst (unify z x subst)))
         (list
          (format #f "Before unify walked value of z: ~a" before-unify)
          (format #f "After unify walked value of z: ~a" (walk z new-subst)))))))
;; ("Before unify walked value of z: #(2)" "After unify walked value of z: 1337")
 )

;; Now that we that unification helper out of the way, we can define our first "goal"
;;
;; Strictly speaking, a "goal" is a function that takes a "state" (defined
;; above) and returns a new state which maybe has some modifications to things
;; like its stream of substitutions.
;;
;; In the case of unification, if we successufly unify then the new returned
;; state will have the extended substitutions. If we fail to unify, then the new
;; returned state will have an empty stream of substitutions.
;;
;; TODO: Clarify: "stream of substitutions" or "stream of states"? Pretty sure I
;; should replace all "stream of substitutions" with "stream of states".
(define empty-stream '())
(define (== u v)
  (lambda (state)
    (let ((new-subst (unify u v (state-subst state))))
      (if new-subst
          (cons (make-state new-subst (state-varid state)) empty-stream)
          empty-stream))))
(comment
 (let ((x (var 0))
       (y (var 1))
       (z (var 2)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (let ((state (vector 'state subst 3))
           (goal (== z x)))
       (goal state))))
 ;; (#(state ((#(2) . 1337) (#(0) . #(1)) (#(1) . 1337)) 3))
 ;;
 ;; Our goal is to unify z with x. Because x is already unified with y, and
 ;; because y is unified with 1337, then z successfully unifies with 1337 and
 ;; our substitution list gets extended.

 (let ((x (var 0))
       (y (var 1))
       (z (var 2)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (let ((state (vector 'state subst 3))
           (goal (== x 42)))
       (goal state))))
 ;; ()
 ;;
 ;; Because x is already associated with y, and y is associated with 1337, then
 ;; x fails to unify with 42. We return the empty stream, because there are no
 ;; valid states.
 )

;; In the comment code bodies above I've been creating variables with `let` and
;; `var` and manually hardcoding and incrementing an identifier number. We
;; should really have a function that does that for us.
(define (call/fresh f)
  (lambda (state)
    (let ((goal (f (var (state-varid state)))))
      ;; The goal received the new var thanks to us passing it to `f' above.
      ;; Now, run the goal with an updated state. (The only update we need to
      ;; make to the state is to increment the var identifier. We don't need to
      ;; add the fresh var from above to the state. The goal itself can do that
      ;; if it wants.)
      (goal (make-state (state-subst state) (add1 (state-varid state)))))))

(comment
 (let ((goal-fresh (call/fresh (lambda (x)
                            (let ((goal-unify (== x 1)))
                              goal-unify)))))
   (goal-fresh empty-state))
 ;; (#(state ((#(0) . 1)) 1))
 ;;
 ;; We see the var `x' as the `#(0)' vector, bound to `1', and the tail of the
 ;; state, the var identifier, incremented to 1.
 ;;
 ;; Regarding the names `goal-fresh' and `goal-unify':
 ;;
 ;; It might seem odd to think of `fresh' as a goal in the same way that you
 ;; think ot `==' as a goal. I think that's because, in plain English, "goal"
 ;; implies some kind of intent or objective, like an intent to unify a var to a
 ;; value. `fresh' doesn't have that kind of intent. But remember, a goal is
 ;; just something that takes a state as input and returns a state (possibly
 ;; updated) as output. I tried to make that clear with the names in this
 ;; comment body. But you'd probably write the above code more like:

 ((call/fresh (lambda (x) (== x 1))) empty-state)
 ;; (#(state ((#(0) . 1)) 1))
 )

;; Now we can get to something fun. Our first "decision"-related goal: `disj'.
;;
;; A disjunction is like an "or" statement. It "succeeds" if either the first
;; goal, or the second goal, or both goals succeed.
;;
;; The resulting stream of states includes every success state.
(define (disj goal1 goal2)
  (lambda (state)
    (stream-append (goal1 state) (goal2 state))))

(define (stream-append stream1 stream2)
  (cond
   ((null? stream1) stream2)
   (else (cons (car stream1) (stream-append (cdr stream1) stream2)))))

(comment
 ((call/fresh (lambda (x)
           (disj
            (== x 1)
            (== x 2))))
  empty-state)
 ;; (#(state ((#(0) . 1)) 1) #(state ((#(0) . 2)) 1))
 ;;
 ;; There's our stream of two possible valid states. The variable `x' can either be 1 or 2.

 ((call/fresh (lambda (x)
           (disj
            (call/fresh (lambda (y)
                     (== x y)))
            (== x 5))))
  empty-state)
 ;; (#(state ((#(0) . #(1))) 2) #(state ((#(0) . 5)) 1))
 )

;; The logically decision subsequent to `or' would be `and', a.k.a `conj'
(define (conj g1 g2)
  (lambda (state)
    (stream-append-map g2 (g1 state))))

(define (stream-append-map goal stream)
  (cond
   ((null? stream) empty-stream)
   (else (stream-append (goal (car stream)) (stream-append-map goal (cdr stream))))))

(comment
 ((call/fresh (lambda (x)
           (conj
            (== x 5)
            (== x 4))))
  empty-state)

 ((call/fresh (lambda (x)
           (call/fresh (lambda (y)
                    (disj
                     (conj
                      (== x 5)
                      (== y 4))
                     (conj
                      (== x 42)
                      (== y 1337)))))))
  empty-state)
 ;; (#(state ((#(1) . 4) (#(0) . 5)) 2) #(state ((#(1) . 1337) (#(0) . 42)) 2))
 )

;; We have a problem though...
(comment

 (define (fives x)
   (disj (== x 5)
         (fives x)))
 ;; When you execute the line below, the execution never terminates. You're
 ;; stuck in an infinite loop.
 ((call/fresh fives) empty-state)

 )

;; We need a way to pause evaluation, hand control back to the user, and let the
;; user continue when they are good and ready. This is where "lazy" streams come
;; in. `disj' uses `stream-append' which is the recursive function that is never
;; yielding control in this case. We can modify `stream-append' to fix that.
(define (stream-append stream1 stream2)
  (cond
   ((null? stream1) stream2)
   ((procedure? stream1) (lambda () (stream-append (stream1) stream2)))
   (else (cons (car stream1) (stream-append (cdr stream1) stream2)))))

;; Now, we can pause the appending process by including a procedure somewhere in
;; one of our streams.

(comment

 (define (fives x)
   (disj (== x 5)
         (lambda (state)
           (lambda ()
             ((fives x) state)))))
 ;; When you execute the line below, the execution never terminates. You're
 ;; stuck in an infinite loop.
 ((call/fresh fives) empty-state)
 ;; (#(state ((#(0) . 5)) 1) . #<procedure>)


 (let ((stream ((call/fresh fives) empty-state)))
   (list
    (car stream)
    (car ((cdr stream)))
    (car ((cdr ((cdr stream)))))))
 ;; (#(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1))
 ;;
 ;; Now we can just keep getting 5s by invoking the cdr of the stream.
 )

;; We need to make a similar modification to the `stream-append-map' used in
;; `conj'.
(define (stream-append-map goal stream)
  (cond
   ((null? stream) empty-stream)
   ((procedure? stream) (stream-append-map goal (stream)))
   (else
    (cons (goal (car stream)) (stream-append-map goal (cdr stream))))))

(comment
 (define (fives x)
   (conj (== x 5)
         (lambda (state)
           (lambda ()
             ((fives x) state)))))
 ((cdr ((call/fresh fives) empty-state)))
 ;; (#(state ((#(0) . 5)) 1) . #<procedure>)
 )

;; There's a technical term for the act of wrapping one of these goals in a
;; function. It's called "inverse-eta-delay". This will be a pretty common task
;; that users will need to do, so let's make a utility.
(define-syntax zzz
  (syntax-rules ()
    ((_ goal) (lambda (state) (lambda () (goal state))))))

(comment
 (define (fives x)
   (conj (== x 5)
         (zzz (fives x))))
 ((cdr ((call/fresh fives) empty-state)))
 ;; (#(state ((#(0) . 5)) 1) . #<procedure>)
 ;;
 ;; Clean!
 )

;; There's one more problem we need to deal with.
;;
;; This is best shown by example.
(comment
 (define (fives x)
   (disj (== x 5)
         (zzz (fives x))))
 (define (sixes x)
   (disj (== x 6)
         (zzz (sixes x))))
 (define (fives-and-sixes x)
   (disj (fives x)
         (sixes x)))
 (let ((goal (call/fresh fives-and-sixes)))
   (list
    (car (goal empty-state))
    (car ((cdr (goal empty-state))))
    (car ((cdr ((cdr (goal empty-state))))))))
;; (#(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1))
;;
;; Even though we want both 5s and 6s, we're going to just keep getting 5s. This
;; isn't a "complete" search strategy. We just keep going deeper and deeper down
;; one branch. We need to occasionally peel off and do some breadth searching.
 )

;; Interleaving streams
;;
;; We can address that issue by modifying `stream-append' again. Instead of
;; always appending the car of stream1 to the recursive call, we can
;; occasionally alternate and append the car of stream2 to the recursive call.
;; This act is called a "binary trampoline".
(define (stream-append stream1 stream2)
  (newline)
  (display "stream-append stream1 and stream2")
  (display stream1)
  (display stream2)
  (newline)
  (cond
   ((null? stream1) stream2)
   ((procedure? stream1) (lambda () (stream-append stream2 (stream1)))) ;; Here's the switcharoo.
   (else (cons (car stream1) (stream-append (cdr stream1) stream2)))))

(comment
 (define (fives x)
   (disj (== x 5)
         (zzz (fives x))))
 (define (sixes x)
   (disj (== x 6)
         (zzz (sixes x))))
 (define (fives-and-sixes x)
   (disj (fives x)
         (sixes x)))
 (let ((goal (call/fresh fives-and-sixes)))
   (list
    (car (goal empty-state))
    (car ((cdr (goal empty-state))))
    (car ((cdr ((cdr (goal empty-state))))))))
;; (#(state ((#(0) . 5)) 1) #(state ((#(0) . 6)) 1) #(state ((#(0) . 5)) 1))
;;                                           |
;; Compare that to the previous:             |-- 6 above vs. 5 previously.
;;                                           |
;; (#(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1) #(state ((#(0) . 5)) 1))
 )

;; Transparency
;;
;; TODO: This section is superseded by the First-Order section that follows.
;;
;; Now the fun part.
;;
;; How do we update this code so that the search can be externally driven?
;;
;; This is as far as I've cleary thought things through, so it gets fuzzy now.
;; I'm going to explore.
;;
;; What would happen if we updated state to include what basically amounts to a
;; call stack, and a list of which decisions we made to get where we are? Then
;; we could condition our future search.
;;
;; In the fives and sixes example, what if, instead of alternating 5s and 6s, we
;; could search the 5s path every 3rd choice? What would the code look like that
;; could support that?
;;
;; Maybe we can start by just data-izing everything, instead of invoking it.
(define (==-t u v)
  (lambda (state)
    `((== ,u ,v) ,state ,(== u v))))

(comment
 (let ((x (var 0))
       (y (var 1))
       (z (var 2)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (let ((state (vector 'state subst 3))
           (goal (==-t z x)))
       (list
        (goal state)
        ((caddr (goal state)) state)))))
 ;; (((== #(2) #(0)) #(state ((#(0) . #(1)) (#(1) . 1337)) 3) #<procedure>)
 ;;  (#(state ((#(2) . 1337) (#(0) . #(1)) (#(1) . 1337)) 3)))
 )

(define (disj-t goal1 goal2)
  (lambda (state)
    `((disj ,goal1 ,goal2)
      ,state
      (,disj ,goal1 ,goal2))))

(comment
 (let ((goal (call/fresh (lambda (x)
                      (disj-t
                       `(==-t x 1)
                       `(==-t x 2))))))
   (list
    (goal empty-state)
    ((caddr (goal empty-state)) empty-state)))
 ;; (#(state ((#(0) . 1)) 1) #(state ((#(0) . 2)) 1))
 ;;
 ;; There's our stream of two possible valid states. The variable `x' can either be 1 or 2.

 ((call/fresh (lambda (x)
           (disj
            (call/fresh (lambda (y)
                     (== x y)))
            (== x 5))))
  empty-state)
 ;; (#(state ((#(0) . #(1))) 2) #(state ((#(0) . 5)) 1))
 )

;;;; First-Order microKanren
;;
;;
;; I'm going to start afresh below.
;;
;; We'll use the knowledge we gained from what we wrote above. But we'll
;; re-write all of the code to more closely match the FOMR paper. The exception
;; will be that we're writing this in Scheme and the paper was written in
;; Racket.
;;
;; De-functionalizing the higher-order representation of goals and streams to a first-order implementation of microKanren
;;
;; What do we need to de-functionalize?
;;
;; The goal constructors, disj, conj, ==, and relate.
;;
;; The stream constructors: stream-append-map, stream-append, and zzz (a.k.a
;; pause in http://minikanren.org/workshop/2019/minikanren19-final2.pdf)
;;
;; In FOMR, this is done by defining the above in terms of structs, so that they
;; can be decomposed with the help of pattern matching. (Mature streams are
;; represented the same way as higher-order microKanren, a pair of an answer and
;; a remaining stream.)

;; Common code

;; Since Chez doesn't give us structs...
(define-syntax defrecord
  (syntax-rules ()
    ((_ name name?)
     (begin
       (define name (vector 'name))
       (define (name? datum) (eq? name datum))))
    ((_ name name? (field set-field) ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (define (define-field-setter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum value)
                    (let ((new (vector-copy datum)))
                      (vector-set! new ,idx value)
                      new))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...))
         (let ((set-fns (range-assoc 1 '(set-field ...))))
           (begin (define-field-setter 'set-field set-fns) ...)))))
    ((_ name name? field ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...)))))))

(defrecord var var? var-name var-index)
(define (var=? v1 v2)
  (eq? (var-index v1) (var-index v2)))
;; Only used in reification? Run? Map? (Search FOMR)
(define initial-var (var #f 0))
;; TODO: For some reason, FOMR doesn't go the functional route.
;; fresh is a lot different.
(define var/fresh
  (let ((index 0))
    (lambda (name)
      (set! index (+ 1 index))
      (var name index))))

(define (make-var/fresh)
  (let ((index 0))
    (lambda (name)
      (set! index (+ 1 index))
      (var name index))))

(comment
 (var/fresh 'bar)
 (let ((v1 (var 'foo 0))
       (v2 (var 'bar 1)))
   (list (var=? v1 v2)
         (var=? v1 v1)))
 (let ((foo 1))
   (set! foo 2)
   foo)
 )

(define empty-substitution '())
(define (walk term substitution)
  (let ((found (and (var? term)
                  (assp (lambda (t) (var=? t term)) substitution))))
    (if found
        (walk (cdr found) substitution)
        term)))
;; TODO: I'm skipping the occurs? check for now.
(define (extend-substitution var val substitution)
  `((,var . ,val) . ,substitution))

;; TODO: Can I refactor this FOMR code to be functional? Can state include the
;; fresh var index?
;;
;; See section 4.1.1 of FOMR.
;;
;;   Without this simplification, a first-order implementation would be burdened
;;   with managing the functional counter to resolve unbound logic variables.
;;
;;   Because we would like to implement many interpreters, this burden would
;;   multiply, because each interpreter would have to includ support for
;;   resolving unbound logic variables.
;;
;; I don't fully understand what that means.
(defrecord state state? state-subst state-varid)
(define empty-state (state empty-substitution 0))

(define (unify v1 v2 substitution)
  (let ((v1 (walk v1 substitution))
        (v2 (walk v2 substitution)))
    (cond
     ((and (var? v1) (var? v2) (var=? v1 v2))
      substitution)
     ((var? v1)
      (extend-substitution v1 v2 substitution))
     ((var? v2)
      (extend-substitution v2 v1 substitution))
     ((and (pair? v1) (pair? v2))
      (let ((substitution (unify (car v1) (var v2) substitution)))
        (and substitution (unify (cdr v1) (cdr v2) substitution))))
     (else (if (eqv? v1 v2)
               substitution)))))

(define (unify-state v1 v2 st)
  (let ((sub (unify v1 v2 (state-subst st))))
    (and sub (cons (state sub 0) #f))))

(comment
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y)))
   (unify-state x 1 (car (unify-state x y empty-state))))

 (pair? (unify-state #t #t empty-state))
 )

(comment
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y))
       (z (var/fresh 'z)))
   (let ((subst `((,x . ,y) (,y . 1337))))
     (let ((before-unify (walk z subst)))
       (let ((new-subst (unify z x subst)))
         (list
          (format #f "Before unify walked value of z: ~a" before-unify)
          (format #f "After unify walked value of z: ~a" (walk z new-subst)))))))

 ;; ("Before unify walked value of z: #(2)" "After unify walked value of z: 1337")
 )

;; Reification
(define (walk* term substitution)
   (let ((term (walk term substitution)))
     (if (pair? term)
         `(,(walk* (car term) substitution) . ,(walk* (cdr term) substitution))
         term)))

(define (reified-index index)
  (string->symbol
   (string-append "_." (number->string index))))

(define (reify term state)
  (define index -1)
  (walk* term (let loop ((term term) (substitution (state-subst state)))
                (define tm (walk term substitution))
                (cond
                 ((pair? tm)
                  (loop (cdr tm) (loop (car tm) substitution)))
                 ((var? tm)
                  (set! index (+ 1 index))
                  (extend-substitution tm (reified-index index) substitution))
                 (else substitution)))))

(define (reify/initial-var state)
  (reify initial-var state))

(comment
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y))
       (z (var/fresh 'z)))
   (let ((state (state `((,initial-var . ,(list x y z)) (,y . (1337 . ,x))) 0)))
     (reify/initial-var state)))
 ;; (_.0 (1337 . _.0) _.1)
 )

;; Copied directly from neuralkanren repo
;;
;; I'm writing this on a plane without wifi, so I don't have access to the Chez
;; docs, so I'm hacking this together from the neuralkanren repo and the FOMR
;; paper.
;;
;; One difference, since we're copying from the neuralkanren repo rather than
;; writing in Racket like the FOMR paper, is that we'll use these defrecord
;; predicates instead of patern matching.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Data model for logic programming with transparent constraint trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read section "3. Standard biased interleaving search interface" to see how
;; a typical miniKanren search implementation maps onto this data model.

;; The full constraint tree grammar consists of:
(defrecord conj conj? conj-c1 conj-c2)
(defrecord disj disj? disj-c1 disj-c2)
(defrecord == ==? ==-t1 ==-t2)
;; * recursive constraints that are currently suspended
(defrecord zzz zzz? zzz-metadata zzz-wake)
;; * subtrees that have not yet propagated equality info (stored in a state)
(defrecord pause pause? pause-state pause-goal)
;; User-defined relations
(defrecord relate relate? relate-thunk relate-description)
;; The interaction system currently only presents constraint trees that are in
;; disjunctive normal form (DNF), and that have propagated all equality
;; information, meaning no `pause` or `==` nodes remain.
(defrecord stream-append stream-append? stream-append-stream-1 stream-append-stream-2)
(defrecord stream-append-map stream-append-map? stream-append-map-stream stream-append-map-goal)
(conj (== 'x 5) (== 'y 9))

;; The FOMR paper uses mature, I think maybe because Racket doesn't have a procedure? function. TODO: Pick one for this Chez Scheme implementation and stick with it.
(define (mature? stream)
  (or (null? stream) (pair? stream)))

(comment
 (let ((stream (pause empty-state (== #t #t))))
   (list (mature? stream)
         (mature? (step stream))))
 ;; (#f #t)
 )
(define (mature stream)
  (if (mature? stream)
      stream
      (mature (step stream))))

;; We are going to need two more utilities: step and start. These are small-step
;; interpreters for streams and goals, respectively.
;;
;; These interpreters work together to implement the interleaving search
;; strategy defined by the higher-order representation.
;;
;; What does `step' do?
;;
;; It evaluates a single thing in our stream. In first-order representation, our
;; "stream" is an unevaluated description of what we want to do. Our
;; `stream-append' record is basically a description, the code, saying that we
;; want to append one stream to another. In higher-order, that would exist as a
;; head and a lazy tail. In first-order, that exists as a head and a `pause'
;; record. That's why these two work in unison.
(define (step record)
  (newline)
  (display "Stepping through record:")
  (display record)
  (newline)
  (cond
   ((stream-append? record)
    (let ((stream-1 (stream-append-stream-1 record))
          (stream-2 (stream-append-stream-2 record)))
      (let ((stream-1 (if (mature? stream-1) stream-1 (step stream-1))))
        (cond
         ((not stream-1) stream-2)
         ((pair? stream-1)
          (cons (car stream-1)
                (stream-append stream-2 (cdr stream-1))))
         (else
          (stream-append stream-2 stream-1))))))
   ((stream-append-map? record)
    (let ((stream (stream-append-map-stream record))
          (goal (stream-append-map-goal record)))
      (newline)
      (display "previous stream")
      (display stream)
      (let ((stream (if (mature? stream) stream (step stream))))
        (display "next stream")
        (display stream)
        (newline)
        ;;(break)
        (cond
         ((not stream) #f)
         ((pair? stream)
          (step (stream-append (pause (car stream) goal)
                               (stream-append-map (cdr stream) goal))))
         (else (stream-append-map stream goal))))))
   ((pause? record)
    (start (pause-state record) (pause-goal record)))
   (else record)))

(comment
 ;; (let ((s1 '(((a . 0) 1)))
 ;;       (s2 '(((b . 0) 1))))
 ;;   (let ((s3 (stream-append s1 s2)))
 ;;     (list s3 (step (step s3)))))
 ;; (#(stream-append (((a . 0) 1)) (((b . 0) 1)))
 ;;  (((a . 0) 1) . #(stream-append (((b . 0) 1)) ())))

 ;; (#(stream-append (((a . 0) 1)) (((b . 0) 1)))
 ;;  (((a . 0) 1) . #(stream-append (((b . 0) 1)) ())))
 )

(comment
 (step (cdr (step (cdr (step (pause empty-state (disj (== #t #t) (== #t #f))))))))

2
 )

(define (start state goal)
  (newline)
  (display "Starting state and goal:")
  (display state)
  (display goal)
  (newline)
  (cond
   ((disj? goal)
    (step (stream-append (pause state (disj-c1 goal))
                         (pause state (disj-c2 goal)))))
   ((conj? goal)
    (step (stream-append-map (pause state (conj-c1 goal))
                             (conj-c2 goal))))
   ((relate? goal)
    (pause state ((relate-thunk goal))))
   ((==? goal)
    (unify-state (==-t1 goal) (==-t2 goal) state))))

(comment
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y))
       (z (var/fresh 'z)))
   (let ((state (state empty-substitution 0))
         (goal1 (== x 5))
         (goal2 (== x y)))
     (start state (disj goal1 goal2))))
 ;; (#(state ((#(var x 12) . 5)) 0))
 ;; #(stream-append #(pause #(state () 0) #(== #(var x 18) #(var y 17)))
 ;;                 #(pause #(state () 0) #(== #(var x 18) 5)))
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y))
       (z (var/fresh 'z)))
   (let ((state (state empty-substitution 0))
         (goal1 (== x 5))
         (goal2 (== x y)))
     (step (start state (disj goal1 goal2)))))
 ;;  #(stream-append #(pause #(state () 0) #(== #(var x 15) 5))
 ;;                  #(pause #(state () 0) #(== #(var x 15) #(var y 14))))
 (let ((x (var/fresh 'x))
       (y (var/fresh 'y))
       (z (var/fresh 'z)))
   (let ((st (state empty-substitution 0))
         (goal1 (== x 5))
         (goal2 (== x y)))
     (step (step (start st (disj goal1 goal2))))))

 ;; (#(== #(var x 39) 5)
 ;;  (#(state ((#(var x 39) . 5)) 0)
 ;;   . #(stream-append
 ;;       #(pause #(state () 0) #(== #(var x 39) #(var y 38)))
 ;;       #f)))



 (let ((var/fresh (make-var/fresh)))
   (let ((x (var/fresh 'x))
         (y (var/fresh 'y))
         (z (var/fresh 'z)))
     (let ((goal1 (== x 5))
           (goal2 (disj
                   (== x y)
                   (== 1337 y))))
       (let ((goal3 (conj goal2 (== z 42))))
         (step (start (state empty-substitution 0) goal1))))))

 ;; (#(state ((#(var x 3) . 5)) 0))
 )

(define (bit-xoro in-1 in-2 out)
  (disj
   (conj (== in-1 0) (conj (== in-2 0) (== out 0)))
   (disj
    (conj (== in-1 1) (conj (== in-2 0) (== out 1)))
    (disj
     (conj (== in-1 0) (conj (== in-2 1) (== out 1)))
     (conj (== in-1 1) (conj (== in-2 1) (== out 0)))))))




(comment
 (let ((var/fresh (make-var/fresh)))
   (let ((x (var/fresh 'x))
         (y (var/fresh 'y)))
     (let ((goal1 (bit-xoro x y 0))
           (state (state empty-substitution 0)))
       (start state (step (start state goal1))))))

 ;; #(stream-append
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(disj #(conj #(== #(var x 3) 1)
 ;;                   #(conj #(== #(var y 2) 0)
 ;;                          #(== #(var z 1) 1)))
 ;;            #(disj #(conj #(== #(var x 3) 0)
 ;;                          #(conj #(== #(var y 2) 1)
 ;;                                 #(== #(var z 1) 1)))
 ;;                   #(conj #(== #(var x 3) 1)
 ;;                          #(conj #(== #(var y 2) 1)
 ;;                                 #(== #(var z 1) 0))))))
 ;;   #(stream-append-map
 ;;     #(pause #(state () 0)
 ;;             #(== #(var x 3) 0))
 ;;     #(conj #(== #(var y 2) 0)
 ;;            #(== #(var z 1) 0))))

 )




(define (bit-xoro in-1 in-2 out)
  (disj
   (conj (== in-1 0) (conj (== in-2 0) (== out 0)))
   (disj
    (conj (== in-1 1) (conj (== in-2 0) (== out 1)))
    (disj
     (conj (== in-1 0) (conj (== in-2 1) (== out 1)))
     (conj (== in-1 1) (conj (== in-2 1) (== out 0)))))))


;;;; Recovering miniKanren
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x call/fresh 'x) ...)
       (conj* g0 gs ...)))))

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (define (name param ...)
       (relate (lambda () (fresh () g ...)) `(,name name ,param ...))))))

(define succeed (== #t #t))
(define fail (== #f #t))

(define-syntax conj*
  (syntax-rules ()
    ((_) succeed)
    ((_ g) g)
    ((_ g0 gs ... g-final) (conj (conj* gs ...) g-final))))

(define-syntax disj*
  (syntax-rules ()
    ((_) fail)
    ((_ g) g)
    ((_ g0 g1 ...) (disj g0 (disj* g1 ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x (var/fresh 'x)) ...)
       (conj* g0 gs ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))

(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((goal (fresh (x ...) (== (list x ...) initial-var) g0 gs ...)))
       (pause empty-state goal)))))

(comment
 (query (x)
        (disj*
         (== x 5)
         (== x 6)))
 ;; #(pause #(state () 0)
 ;;         #(conj #(== #t #t)
 ;;                #(disj #(== #(var x 2) 5)
 ;;                       #(== #(var x 2) 6))))
 (expand '(query (x)
           (disj*
            (== x 5)
            (== x 6))))
 (let ((#{goal mqxipzqu9sw6yqslxn1021rib-4}
        (let ((#{x mqxipzqu9sw6yqslxn1021rib-5}
               (var/fresh (quote x))))
          (conj succeed
                (disj (== #{x mqxipzqu9sw6yqslxn1021rib-5} 5)
                      (== #{x mqxipzqu9sw6yqslxn1021rib-5} 6))))))
   (pause empty-state #{goal mqxipzqu9sw6yqslxn1021rib-4}))

 (expand
  '(conj*
    (conj*)
    (disj*
     (== x 5)
     (== x 6))))

 )

(define (stream-take n stream)
  (display stream)
  (if (eqv? 0 n)
      '()
      (let ((stream (mature stream)))
        (display stream)
        (if (pair? stream)
            (cons (car stream) (stream-take (and n (- n 1)) (cdr stream)))
            '()))))

(comment

 (stream-take 1 (query (x)
                       (disj*
                        (== x 5)
                        (== x 6))))

 )


(define-syntax run
  (syntax-rules ()
    ((_ n body ...)
     (map reify/initial-var (stream-take n (querjy body ...))))))

(define-syntax run*
  (syntax-rules ()
    ((_ body ...)
     (run #f body ...))))

(comment

 (define-relation (bit-xoro x y r)
   (conde
    ((== 0 x) (== 0 y) (== 0 r))
    ((== 0 x) (== 1 y) (== 1 r))
    ((== 1 x) (== 0 y) (== 1 r))
    ((== 1 x) (== 1 y) (== 0 r))))

 (fresh (x y r)
   (== r 0)
   (bit-xoro x y r))
 ;; #(conj #(== #t #t) #(relate #<procedure> (#<procedure bit-xoro> bit-xoro #(var x 9) #(var y 8) #(var r 7))))


 (expand (bit-xoro x y 0))
 (run* (x y r) (bit-xoro x y 0))
 ((0 0 _.0) (1 1 _.0))

 (define-relation (bit-ando x y r)
   (conde
    ((== 0 x) (== 0 y) (== 0 r))
    ((== 0 x) (== 1 y) (== 0 r))
    ((== 1 x) (== 0 y) (== 0 r))
    ((== 1 x) (== 1 y) (== 1 r))))

 (define-relation (half-addero x y r c)
   (bit-xoro x y r)
   (bit-ando x y c))

 (run 8 (x y r c) (half-addero 1 0 r c))

 (define-relation (full-addero c-in x y r c-out)
   (call/fresh (r0 c0 c1)
     (half-addero c-in x r0 c0)
     (half-addero r0 y r c1)
     (bit-xoro c0 c1 c-out)))

 (run 20 (c-in x y r c-out)
   (full-addero c-in x y r c-out)))

(define-relation (appendo l s ls)
  (conde
   ((== l '()) s)
   ((fresh (a d r))
    (== a (car l))
    (== a (car ls))
    (== d (cdr l))
    (== r ls)
    (appendo d s r))))

(comment
 (let ((g (query (x y) (disj (== x y) (== x 1)))))
   (let ((g (step g)))
     g))

 )

(comment

 (let ((g (query (x y) (disj (== x y) (== x 1)))))
   g)

 ;; #(pause
 ;;   #(state () 0)
 ;;   #(conj
 ;;     #(== #t #t)
 ;;     #(disj
 ;;       #(== #(var x 10) #(var y 9))
 ;;       #(== #(var x 10) 1))))

 (let ((g (query (x y) (disj (== x y) (== x 1)))))
   (let ((g (step g)))
     g))

 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(disj
 ;;     #(== #(var x 12) #(var y 11))
 ;;     #(== #(var x 12) 1)))

 (let ((g (query (x y) (disj (== x y) (== x 1)))))
   (let ((g (step g)))
     (let ((g (step g)))
       g)))

 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(disj
 ;;     #(== #(var x 14) #(var y 13))
 ;;     #(== #(var x 14) 1)))

 (let ((g (query (x y) (disj (== x y) (== x 1)))))
   (let ((g (step g)))
     (let ((g (step g)))
       (step g))))

 (let ((g (query (x y) (appendo x y '(1 2 3)))))
   g)

 ;; #(pause
 ;;   #(state () 0)
 ;;   #(conj
 ;;     #(== #t #t)
 ;;     #(relate
 ;;       #<procedure>
 ;;       (#<procedure appendo> appendo #(var x 13) #(var y 12) (1 2 3)))))

 (let ((g (query (x y) (appendo x y '(1 2 3)))))
   (let ((g (step g)))
     g))
 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(relate #<procedure> (#<procedure appendo> appendo #(var x 2) #(var y 1) (1 2 3))))

 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(relate #<procedure> (#<procedure appendo> appendo #(var x 15) #(var y 14) (1 2 3))))

 (let ((g (query (x y) (appendo x y '(1 2 3)))))
   (let ((g (step g)))
     (let ((g (step g)))
       g)))
 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(relate #<procedure> (#<procedure appendo> appendo #(var x 4) #(var y 3) (1 2 3))))

 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(relate #<procedure> (#<procedure appendo> appendo #(var x 29) #(var y 28) (1 2 3))))

 (let ((g (query (x y) (appendo x y '(1 2 3)))))
   (let ((g (step g)))
     (let ((g (step g)))
       (let ((g (step g)))
         g))))

 ;; #(stream-append-map
 ;;   #(pause
 ;;     #(state () 0)
 ;;     #(== #t #t))
 ;;   #(relate #<procedure> (#<procedure appendo> appendo #(var x 31) #(var y 30) (1 2 3))))

 )
