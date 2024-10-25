#lang racket/base

(provide simplify)

(require (only-in racket/match match)
         first-order-miniKanren/microk-fo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propagate shallow constraints and prune failures.
;;
;; > Another interesting property of DNF is that if we apply the full pruning
;; > transformation on a DNF stream, all remaining `==` constraints will be
;; > trivial in the sense that they are guaranteed to succeed, and will not
;; > provide new information. This is because all available equality information
;; > will have already been gathered in a `pause` state. These trivial constraints
;; > may be safely removed.
;; >
;; > - http://minikanren.org/workshop/2019/minikanren19-final2.pdf#page=14
;;
;; TODO: Why exacly is that? Why will "all available equality information" have
;; "been gathered in a `pause` state"?
;; DONE: See "note 42".
(define (prune/stream s)
  (match s
    ((mplus s1 s2) (match (prune/stream s1)
                     (#f (prune/stream s2))
                     (s1 (match (prune/stream s2)
                           (#f s1)
                           (s2 (mplus s1 s2))))))
    ((bind s g)    (match (prune/stream s)
                     (#f          #f)
                     (`(,st . #f) (prune/goal st g))
                     ((pause st g1)
                      (match (prune/goal st g)
                        (#f           #f)
                        ((pause st g) (pause st (conj g1 g)))))
                     (s (match (prune/goal empty-state g)
                          (#f          #f)
                          ((pause _ _) (bind s g))))))
    ((pause st g)  (prune/goal st g))
    (`(,st . ,s)   `(,st . ,(prune/stream s)))
    (s             s)))

(define (prune/goal st g)
  (define (prune/term t) (walk* t (state-sub st)))
  (match g
    ((disj g1 g2)
     (match (prune/goal st g1)
       (#f (prune/goal st g2))
       ((pause st1 g1)
        (match (prune/goal st g2)
          (#f           (pause st1 g1))
          ((pause _ g2) (pause st (disj g1 g2)))))))
    ((conj g1 g2)
     (match (prune/goal st g1)
       (#f            #f)
       ((pause st g1) (match (prune/goal st g2)
                        (#f            #f)
                        ((pause st g2) (pause st (conj g1 g2)))))))
    ((relate thunk d) (pause st (relate thunk (prune/term d))))
    ;; note 42
    ;; Any time we reach an `==` constraint, we match on its unification.
    ;; If it doesn't unify, it's pruned to `#f`.
    ;; If it does    unify, it's replaced with a `pause`.
    ;; Therefore, it's either trivially failure or success. We no longer need to
    ;; check to see if it's unified. It gets unified as part of this pruning.
    ((== t1 t2)
     (let ((t1 (prune/term t1)) (t2 (prune/term t2)))
       (match (unify t1 t2 st)
         (#f          #f)
         (st          (pause st (== t1 t2))))))
    ((=/= t1 t2)
     (let ((t1 (prune/term t1)) (t2 (prune/term t2)))
       (match (disunify t1 t2 st)
         (#f          #f)
         (st          (pause st (=/= t1 t2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform into Disjunctive Normal Form.
(define (dnf/stream s)
  (define (push-pause st g)
    (match g
      ((disj g1 g2) (mplus (push-pause st g1) (push-pause st g2)))
      (g            (pause st g))))
  (match s
    ((bind s g)
     (let loop1 ((s (dnf/stream s)) (g (dnf/goal g)))
       (define (loop2 s g)
         (match g
           ((disj ga gb) (mplus (loop2 s ga) (loop2 s gb)))
           (g            (bind s g))))
       (match s
         ((mplus sa sb) (mplus (loop1 sa g) (loop1 sb g)))
         (`(,st . ,s)   (mplus (push-pause st g) (loop1 s g)))
         (s             (loop2 s g)))))
    ((pause st g)  (push-pause st (dnf/goal g)))
    ((mplus s1 s2) (mplus (dnf/stream s1) (dnf/stream s2)))
    (`(,st . ,s)   `(,st . ,(dnf/stream s)))
    (s             s)))

(define (dnf/goal g)
  (match g
    ((conj g1 g2)
     (let loop1 ((g1 (dnf/goal g1)) (g2 (dnf/goal g2)))
       (define (loop2 g1 g2)
         (match g2
           ((disj g2a g2b) (disj (loop2 g1 g2a) (loop2 g1 g2b)))
           (g2             (conj g1 g2))))
       (match g1
         ((disj g1a g1b) (disj (loop1 g1a g2) (loop1 g1b g2)))
         (g1             (loop2 g1 g2)))))
    ((disj g1 g2) (disj (dnf/goal g1) (dnf/goal g2)))
    (g            g)))

(define (simplify s)
  (prune/stream (dnf/stream s)))
