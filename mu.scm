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
     (lambda () 'noop))))

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
            #f)))
     (else (if (eqv? u v)
               subst
               #f)))))

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
(define (fresh f)
  (lambda (state)
    (let ((goal (f (var (state-varid state)))))
      ;; The goal received the new var thanks to us passing it to `f' above.
      ;; Now, run the goal with an updated state. (The only update we need to
      ;; make to the state is to increment the var identifier. We don't need to
      ;; add the fresh var from above to the state. The goal itself can do that
      ;; if it wants.)
      (goal (make-state (state-subst state) (add1 (state-varid state)))))))

(comment
 (let ((goal-fresh (fresh (lambda (x)
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

 ((fresh (lambda (x) (== x 1))) empty-state)
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
 ((fresh (lambda (x)
           (disj
            (== x 1)
            (== x 2))))
  empty-state)
 ;; (#(state ((#(0) . 1)) 1) #(state ((#(0) . 2)) 1))
 ;;
 ;; There's our stream of two possible valid states. The variable `x' can either be 1 or 2.

 ((fresh (lambda (x)
           (disj
            (fresh (lambda (y)
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
 ((fresh (lambda (x)
           (conj
            (== x 5)
            (== x 4))))
  empty-state)

 ((fresh (lambda (x)
           (fresh (lambda (y)
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
 ((fresh fives) empty-state)

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
 ((fresh fives) empty-state)
 ;; (#(state ((#(0) . 5)) 1) . #<procedure>)


 (let ((stream ((fresh fives) empty-state)))
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
 ((cdr ((fresh fives) empty-state)))
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
 ((cdr ((fresh fives) empty-state)))
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
 (let ((goal (fresh fives-and-sixes)))
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
 (let ((goal (fresh fives-and-sixes)))
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
