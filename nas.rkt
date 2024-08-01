#lang racket/base

(provide run
         stream->choices
         make-nastep
         layerso
         parse-nums)

(require (prefix-in dispatch: web-server/dispatch)
         (prefix-in dispatch-log: web-server/dispatchers/dispatch-log)
         (prefix-in jsexpr: web-server/http/json)
         (prefix-in xexpr: web-server/http/xexpr)
         (prefix-in rqstruct: web-server/http/request-structs)
         (prefix-in servlet: web-server/servlet-env)
         (only-in racket/match match)
         (only-in racket/list takef dropf)
         (only-in racket/port with-output-to-string)
         json
         first-order-miniKanren/microk-fo
         first-order-miniKanren/tools
         "math.rkt"
         (rename-in "mk-syntax.rkt" (run mk/run))
         racket/serialize)

(define (not-found-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Uh-oh! Page not found.")))))

(define (home-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Look ma, no state!!!!!!!!!")))))

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
      (== a `(,a1 . ,a2))
      (== ab `(,a1 . ,res))
      (appendo a2 b res)))))

(define (stream->choices s)
  (let loop ((s (prune/stream (dnf/stream s))))
    (match s
      ((mplus s1 s2) (append (loop s1) (loop s2)))
      (#f            '())
      (`(,st . ,s)   (cons st (loop s)))
      (s             (list s)))))

(define (initialize request)
  (let ((choices
         (stream->choices
          (prune/stream
           (dnf/stream
            (pause
             empty-state
             (fresh (a b)
                    (== `(,a ,b) initial-var)
                    (appendo a b `(1 2 3 4)))))))))
    (jsexpr:response/jsexpr (hash 'choices (with-output-to-string (lambda ()
                                                                    (write (serialize (serialize-choices choices)))))))))


(define (serialize-choices choices)
  (format "CHOICES: ~a" (map serialize-choice choices)))

;; stream->choices returns:
;;   - list of bind
;;   - empty list
(define (serialize-choice choice)
  (match choice
    ((pause st g)
     (string-append "(pause "
                    (serialize-state st)
                    " "
                    (serialize-goal g)
                    ")"))))

(define (serialize-state st)
  (let ((sub (state-sub st))
        (diseq (state-diseq st))
        (types (state-types st))
        (distypes (state-distypes st)))
    (format "(state ~a () () ())" (serialize-sub sub))))

(define (intersperse lst sep)
  (if (= 1 (length lst))
      lst
      `(,(car lst) ,sep . ,(intersperse (cdr lst) sep))))

(define (serialize-sub sub)
  (format "(~a)" (apply string-append (intersperse (map serialize-term sub) " "))))

(define (serialize-var v)
  (format "(var ~a ~a)"
          (or (var-name v) "#f")
          (var-index v)))

(define (serialize-term t)
  (cond
    ((var? t) (serialize-var t))
    ((and (pair? t) (not (pair? (cdr t))))
     (format "(~a . ~a)" (serialize-term (car t)) (serialize-term (cdr t))))
    ((and (pair? t) (pair? (cdr t)))
     (format "(~a ~a)" (serialize-term (car t)) (serialize-list-of-terms (cdr t))))
    ((symbol? t) (format "(symbol ~a)" (symbol->string t)))
    ((number? t) (number->string t))
    ((null? t) "()")
    (else (let ()
            (raise 'unhandled)))))

(define (serialize-list-of-terms lst)
  (cond
    ((list? lst)
     (format "~a" (apply string-append (intersperse (map serialize-term lst) " "))))
    ((pair? lst)
     (format "(~a . ~a)" (serialize-term (car lst)) (serialize-term (cdr lst))))
    (else (raise 'unhandled))))

(define (serialize-goal g)
  (match g
    ((conj g1 g2) (format "(conj ~a ~a)" (serialize-goal g1) (serialize-goal g2)))
    ((relate thnk desc) (format "(relate ~a)" (cdr desc)))
    ((== t1 t2) (format "(== ~a ~a)" (serialize-term t1) (serialize-term t2)))))

(define (start-server req)
  (let ((choices (serialize-choices
                  (stream->choices
                   (parallel-step
                    (pause
                     empty-state
                     (fresh (a b)
                            (== `(,a ,b) initial-var)
                            (appendo a b '(1 2 3 4)))))))))
    (jsexpr:response/jsexpr
     (hash 'choices choices))))

;; (let ((choices (serialize-choices
;;                   (stream->choices
;;                    (parallel-step
;;                     (pause
;;                      empty-state
;;                      (fresh (a b)
;;                             (== `(,a ,b) initial-var)
;;                             (appendo a b '(1 2 3 4)))))))))
;;     (hash 'choices choices))

;; (let ((choices (serialize-choices
;;                 (stream->choices
;;                  (parallel-step
;;                   (pause
;;                    empty-state
;;                    (fresh (a b)
;;                           (== `(,a ,b) initial-var)
;;                           (appendo a b '(1 2 3 4)))))))))
;;   (hash 'hint choices))


(define (run!)
  (servlet:serve/servlet start
                 #:port 9999
                 #:servlet-path "/"
                 #:command-line? #t))

;; (run!)



(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(define (one? n)
  (= n 1))

(define (unbuild-num n)
  (let loop ((n n) (r 0) (i 0))
    (cond
     ((null? n) r)
     ((one? (car n)) (loop (cdr n) (+ r (expt 2 i)) (add1 i)))
     (else (loop (cdr n) r (add1 i))))))

(define parse-nums
  (lambda (layers)
    (cond
     ((null? layers) '())
     ((symbol? (car layers))
      (cons (car layers) (map unbuild-num (cdr layers))))
     ((pair? layers)
      (cons (parse-nums (car layers)) (parse-nums (cdr layers))))
     (else layers))))

(define linearo
  (lambda (q in out)
    (== q `(Linear ,in ,out))))

(define percento
  (lambda (x)
    (fresh (y)
      (pluso x y (build-num 99)))))

(define dropouto
  (lambda (q in out)
    (fresh (p)
      (percento p)
      (== in out)
      (== q `(Dropout ,in ,out ,p)))))

(define reluo
  (lambda (q in out)
    (conde
     ((== in out)
      (== q `(Relu ,in ,out))))))


(define conso
  (lambda (head tail result)
    (== result `(,head . ,tail))))

(define-relation (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(define-relation (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(define-relation (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(define-relation (layero layer in out)
  (conde
   ((== layer `(Linear ,in ,out)))
   ((== layer `(Relu ,in ,out))
    (== in out))
   ((fresh (percent)
      (== layer `(Dropout ,in ,out ,percent))
      (== in out)
      (countero percent)
      (<o '(1) percent)
      (<o percent '(0 0 1 0 0 1 1))))))

(define-relation (tailo lst tail)
  (fresh (a d rst)
    (conde
     ((== lst `(,a . ,d))
      (disj*
       (== a 1)
       (== a 0))
      (== d `(,tail . ())))
     ((== lst `(,a . ,rst))
      (disj*
       (== a 1)
       (== a 0))
      (tailo rst tail)))))

(define-relation (countero q)
  (<lo '() q)
  (conde
   ((== q `(1)))
   ((tailo q 1))))

(define-relation (dimso q)
  (<lo '() q)
  (conde
   ((== q `(1)))
   ((tailo q 1))))

(define-relation (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(define-relation (layerso layers in out)
  (conde
   ((fresh (layer)
           (layero layer in out)
           (== layers `(,layer (OUT ,out)))))
   ((fresh (layer-1 layer-2 hidden)
           (dimso hidden)
           (layero layer-1 in hidden)
           (layerso layer-2 hidden out)
           (== layers `(,layer-1 . ,layer-2))))))

(define (identity x) x)

;; `query` binds given vars to `initial-var` and wraps goal in `(pause empty-state ...)`
;; (define ow-stream (query (q) (layerso q '(0 0 0 1) '(0 0 1))))
;; ow-stream

;; Turns `bind` into `mplus` and paused disjunctions into mplus paused disjunctions
;; (define ow-dnf-stream (dnf/stream ow-stream))
;; ow-dnf-stream

;; At every step, we need to print the choices and the chosen.
;; That's what we need for a training run.
;; The last line can just be the architecture.

;; (run* (a b)
;;   (appendo a b '(1 2 3 4)))

(define (mature/step step s)
  (if (mature? s) s (mature/step step (step s))))

(define (stream-take/step step n s)
  (if (eqv? 0 n) '()
      (let ((s (mature/step step s)))
        (if (pair? s)
            (cons (car s) (stream-take/step step (and n (- n 1)) (cdr s)))
            '()))))

;; Parallel step
(define (ow-parallel-step-simple s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (simplify (ow-parallel-step-simple s1)))))
       (cond ((not s1)   s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (simplify (ow-parallel-step-simple s)))))
       (cond ((not s)   #f)
             ((pair? s)
              (simplify (ow-parallel-step-simple (mplus (pause (car s) g)
                                                        (bind (cdr s) g)))))
             (else      (bind s (ow-parallel-expand g))))))
    ((pause st g) (ow-parallel-start st g))
    (_            s)))

(define (ow-parallel-start st g)
    (match g
      ((disj g1 g2)     (mplus (pause st g1)
                               (pause st g2)))
      ((conj g1 g2)     (bind (pause st g1) g2))
      ((relate thunk _) (pause st (thunk)))
      ((== t1 t2)       (state->stream (unify t1 t2 st)))))

(define (ow-parallel-expand g)
    (let loop ((g g))
      (match g
        ((conj g1 g2)     (conj (loop g1) (loop g2)))
        ((relate thunk _) (thunk))
        (_                g))))

(define (ow-parallel-step s)
  (ow-parallel-step-simple s))

(define (serialize-results r)
  (format "RESULTS: ~a" (map reify/initial-var r)))

;; At each step
;;   Log the choice. The choice includes the state, so it has everything you need to make your decision.
;;     In other words, you don't need a history of choices.
;;
;;   If there's results, log the results.
;;
;; Training data will be the list of choice+ result
(define (ow-step s)
  (with-output-to-file "/tmp/out"
    (lambda ()
      (let ((s (stream->choices (parallel-step s))))
        (let ((cxs (choices s))
              (rts (results s)))
          (newline)
          (when (not (null? cxs)) (println (serialize-choices cxs)))
          (when (not (null? rts)) (println (serialize-results rts)))
          (newline)))
          (step s))
    #:exists 'append))

(define (choices stream)
  (dropf stream state?))

(define (results stream)
  (takef stream state?))

(define-syntax run/step-simplify
  (syntax-rules ()
    ((_ step n body ...) (stream-take/step
                          (lambda (s) (simplify (step s))) n (simplify (query body ...))))))

;; (let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
;;   (map reify/initial-var (stream-take/step (lambda (s) (simplify (ow-step s))) 4 (simplify stream))))

;;;; TODO
;; make your serialized choices as brief as explore's

;; (let ((stream (query (q) (layerso q (build-num 768) (build-num 10)))))
;;   (parse-nums (map reify/initial-var (stream-take/step (lambda (s) (simplify (ow-step s))) 3 (simplify stream)))))

;; (define (rsstep s)
;;   (simplify (random-step (simplify (step s)))))

;; (let ((stream (query (q) (layerso q (build-num 8) (build-num 3)))))
;;   (simple-step (list-ref (stream->choices (simple-step (simple-step stream))) 1)))

;; (map reify/initial-var
;;      (let ((stream (query (q) (layerso q (build-num 8) (build-num 3)))))
;;        (let loop ((stream (stream->choices stream))
;;                   (i 100))
;;          (let ((cxs (choices stream))
;;                (rxs (results stream)))
;;            (cond
;;              ((null? cxs) rxs)
;;              ((not (null? rxs))
;;               (cons rxs (loop (stream->choices (cdr stream)) (- i 1))))
;;              ((eqv? 0 i) '())
;;              (else (let ((stream (step (list-ref cxs (random 0 (length cxs))))))
;;                      (cond
;;                        ((pair? stream)
;;                         (cons (car stream) (loop (stream->choices (cdr stream)) (- i 1))))
;;                        ((null? stream) '())
;;                        (else
;;                         (loop (stream->choices stream) (- i 1)))))))))))

;; (let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
;;  (list
;;   (serialize-choices (stream->choices (choices (parallel-step stream))))
;;   (serialize-choices (stream->choices (choices (parallel-step (parallel-step stream)))))))

;; (run/step-simplify ow-step 2 (a b) (appendo a b '(1 2 3 4)))
;; (let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
;;  (list
;;   (serialize-choices (stream->choices (parallel-step stream)))
;;   (parallel-step (parallel-step (parallel-step stream)))
;;   (serialize-choices (stream->choices (parallel-step (parallel-step (parallel-step stream)))))))

;; Using parallel-step allows us to expand constraints from both examples
;; simultaneously, pruning impossibilities much sooner.

;; lengtho so we don't run forever.
(define-relation (lengtho l n)
  (conde
   ((== l '()) (== n '()))
   ((fresh (head tail result)
           (== `(,head . ,tail) l)
           (pluso '(1) result n)
           (lengtho tail result)))))

;; (run* (a b)
;;   (appendo a b '(1 2 3 4)))

;; (let ((stream (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
;;   (list
;;    stream
;;    (step stream)
;;    (parallel-step stream)
;;    (prune/stream (dnf/stream (parallel-step stream)))
;;    (stream->choices (step stream))
;;    (dnf/stream (mplus-mplus-s2 (prune/stream (dnf/stream (parallel-step stream)))))))

;; (let ((stream (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
;;   (prune/stream (dnf/stream (parallel-step stream))))

;; (let ((choices (serialize-choices
;;                   (stream->choices
;;                    (parallel-step
;;                     (pause
;;                      empty-state
;;                      (fresh (a b)
;;                             (== `(,a ,b) initial-var)
;;                             (appendo a b '(1 2 3 4)))))))))
;;     choices)

;; (let ((stream (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
;;   (serialize-choices (stream->choices (prune/stream (dnf/stream stream)))))

;; (let ((stream (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
;;   (ow-step (prune/stream (dnf/stream stream))))

;; (let ((stream (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
;;   (prune/stream (dnf/stream stream)))

;; (parse-nums
;;  (ow-run 1 (q)
;;          (layerso q (build-num 768) (build-num 10))))

;; (with-output-to-file "/tmp/out"
;;   (lambda ()
;;     (for-each
;;      (lambda (arch)
;;        (write (car arch))
;;        (newline))
;;      (parse-nums
;;       (run 10 (q)
;;            (layerso q (build-num 768) (build-num 10))))))
;;   #:exists 'replace)

(define (choose s)
  (random 0 2))

(define (reorder s n)
  (match s
    ((mplus s1 s2)
     (if (= n 0)
         s
         (mplus s2 s1)))
    ((pause st g) (pause st g))))

(define (random-step s)
  (let ((s (simplify s)))
    (if s (step (reorder s (choose s))) s)))

;; (run*/step random-step (a b) (appendo a b '(1 2 3 4)))
;; (parse-nums (run/step random-step 50 (q) (layerso q '(0 0 0 1) '(0 0 1))))

;; (stream->choices (prune/stream (dnf/stream (query (q)
;;                                                   (fresh (x y)
;;                                                          (conj*
;;                                                           (disj*
;;                                                            (== x 1)
;;                                                            (== x 5))
;;                                                           (== y 2)
;;                                                           (== q `(,x . ,y))))))))

;; (drop (run 5000 (q)
;;       (layerso q (build-num 2) (build-num 2))) 4980)

;; (run 20 (q) (layerso q '(0 1) '(0 1)))

;; (drop
;;  (run 100 (q)
;;       (archo q (build-num 2) (build-num 2)))
;;  80)

(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((goal (fresh (x ...) (== (list x ...) initial-var) g0 gs ...)))
       (pause empty-state goal)))))

;; What is the default `step`?
;; (define (step s)
;;   (match s
;;     ((mplus s1 s2) ...)
;;     ((bind s g) ...)
;;     ((pause st g) ...)
;;     (_            s)))
;; It interleaves s1 and s2 in the case of mplus.
;;
;; But really we can define step however we want.
;; We could depth-first stream 1.
;; We could completely ignore stream 1, dropping it, excluding it from results.
;;
;; It all depends on what our product requirements are.
;; Nobody is telling us that we _must_ be a complete search.
;;
;; In the case of generating data for a neural architecture search,
;; is it important that we can backtrack?
;;
;; Maybe a simpler alternative it to order the choices and then iterate over all of them.
(require zeromq)


;; (define (make-requester)
;;   (lambda (s)
;;     (let ((out (open-output-string)))
;;       (write s out)
;;       (zmq-send requester (get-output-string out))
;;       (let ((received (zmq-recv-string requester)))
;;         (printf "received ~s~n" received)
;;         received))))

(define (nastart step st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

(define (make-nastep decide)
  (define (nastep s)
    (match s
      ((mplus s1 s2)
       (let ((s1 (if (mature? s1) s1 (nastep s1))))
         (cond ((not s1) s2)
               ((pair? s1)
                (cons (car s1)
                      (if (eqv? 's1 (decide s))
                          (begin
                            (mplus (cdr s1) s2))
                          (begin
                            (mplus s2 (cdr s1))))))
               (else (if (eqv? 's1 (decide s))
                         (begin
                           (mplus s1 s2))
                         (begin
                           (mplus s2 s1)))))))
      ((bind s g)
       (let ((s (if (mature? s) s (nastep s))))
         (cond ((not s) #f)
               ((pair? s)
                (nastep (mplus (pause (car s) g)
                               (bind (cdr s) g))))
               (else (bind s g)))))
      ((pause st g) (nastart nastep st g))
      (_            s)))
  nastep)

(define (ow-stream-take/step step n qvars s)
  (if (eqv? 0 n)
      '()
      ;; mature is going to make a bunch of decisions, then it's going to be done.
      ;; each iteration of _this_ recursion will be an entire training sample.
      ;; the step function that you pass to mature is going to be the neural network
      (let ((s (mature/step step s)))
        (if (pair? s)
            (cons (car s) (ow-stream-take/step step (and n (- n 1)) qvars (cdr s)))
            '()))))

(define-syntax search
  (syntax-rules (query)
    ((_ step n (query (qvars ...) body ...))
     (begin (printf "Using step procedure: ~s~nExploring query:~n~s~n"
                    'step '(query (qvars ...) body ...))
            (map reify/initial-var (ow-stream-take/step step (and n (- n 1)) '(qvars ...) (query (qvars ...) body ...)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propagate shallow constraints and prune failures.
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




(define (start/step step st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

(define (choose/random s1 s2)
  (if (= 0 (random 0 2))
      `(,s1 . ,s2)
      `(,s2 . ,s1)))


(define (make-stepper choose)
  (define (stepper s)
    (match s
      ((mplus s1 s2)
       (let ((choices (choose s1 s2)))
         (printf "Expanding ~s~n" (car choices))
         (let ((s1 (step (car choices)))
               (s2 (cdr choices)))
           (cond ((not s1) s2)
                 ((pair? s1)
                  (cons (car s1)
                        (mplus s2 (cdr s1))))
                 (else (mplus s2 s1))))))
      ((bind s g)
       (let ((s (stepper s)))
         (cond ((not s) #f)
               ((pair? s)
                (stepper (mplus (pause (car s) g)
                                (bind (cdr s) g))))
               (else (bind s g)))))
      ((pause st g) (start/step stepper st g))
      (_            s)))
  stepper)


(define (take-path d s1 s2)
  (if (= d 0)
      `(,s1 . ,s2)
      `(,s2 . ,s1)))

(define (path-stepper path)
  (define (stepper s)
    (let ((inner-stepper (lambda (s) ((path-stepper (cdr path)) s))))
      (match s
        ((mplus s1 s2)
         (let ((path (cdr path))
               (choices (take-path (car path) s1 s2)))
           (printf "Expanding ~s~n" (car choices))
           (let ((s1 (step (car choices)))
                 (s2 (cdr choices)))
             (cond ((not s1) s2)
                   ((pair? s1)
                    (cons (car s1)
                          (mplus s2 (cdr s1))))
                   (else (mplus s2 s1))))))
        ((bind s g)
         (let ((s (stepper s)))
           (cond ((not s) #f)
                 ((pair? s)
                  (stepper (mplus (pause (car s) g)
                                  (bind (cdr s) g))))
                 (else (bind s g)))))
        ((pause st g) (start/step inner-stepper st g))
        (_            s))))
  stepper)

(let ((mustep (path-stepper '(0 0 0)))
      (s (simplify (query (q) (layerso q '(0 0 1) '(0 1))))))
  (let ((simple-stepper (lambda (s) (simplify (mustep s)))))
    (list
     s
     (simple-stepper s)
     (simple-stepper (simple-stepper s))
     (simple-stepper (simple-stepper (simple-stepper s)))
     (simple-stepper (simple-stepper (simple-stepper (simple-stepper s))))
     (simple-stepper (simple-stepper (simple-stepper (simple-stepper (simple-stepper s))))))))

(define (spaces i)
  (make-string i #\space))

(define (indent i)
  (+ i 2))

(define (pp/mplus i s1 s2)
  (format "mplus~n~a~a"
          (pp/stream (indent i) s1)
          (pp/stream (indent i) s2)))

(define (pp/bind i s g)
  (format "bind~n~a~a"
          (pp/stream (indent i) s)
          (pp/goal (indent i) g)))

(define (pp/disj i g1 g2)
  (format "disj~n~a~a"
          (pp/goal (indent i) g1)
          (pp/goal (indent i) g2)))

(define (flatten/conj c)
  (match c
    ((conj g1 g2)
     (cond
       ((and (conj? g1) (conj? g2))
        (append (flatten/conj g1)
                (flatten/conj g2)))
       ((conj? g1)
        (cons g2 (flatten/conj g1)))
       ((conj? g2)
        (cons g1 (flatten/conj g2)))
       (else (list g1 g2))))
    (c (list c))))

(define (pp/conj- i g1 g2)
  (format "conj~n~a~a"
          (pp/goal (indent i) g1)
          (pp/goal (indent i) g2)))

(define (pp/conj i g1 g2)
  (let ((cxs (flatten/conj (conj g1 g2))))
    (let loop ((cxs cxs)
               (out (format "conj~n")))
      (if (null? cxs)
          out
          (loop (cdr cxs) (string-append out (pp/goal (indent i) (car cxs))))))))

(define (pp/== i t1 t2)
  (format "== ~a ~a~n"
          (pp/term t1)
          (pp/term t2)))

(define (pp/term t)
  (format "~a" t))

(define (pp/relate i t d)
  (format "~a~n" d))

(define (pp/goal i g)
  (match g
    ((disj g1 g2) (format "~a~a" (spaces i) (pp/disj i g1 g2)))
    ((conj g1 g2) (format "~a~a" (spaces i) (pp/conj i g1 g2)))
    ((== t1 t2) (format "~a~a" (spaces i) (pp/== i t1 t2)))
    ((relate t d) (format "~a~a" (spaces i) (pp/relate i t d)))))

(define (pp/pause i st g)
  (format "pause~n~a~a~a"
          (spaces (indent i))
          (pp/state (indent i) st)
          (pp/goal (indent i) g)))

(define (pp/state i st)
  (format "~a~n" st))

(define (pp/stream i s)
  (match s
    ((mplus s1 s2) (format "~a~a" (make-string i #\space) (pp/mplus i s1 s2)))
    ((bind s g) (format "~a~a" (make-string i #\space) (pp/bind i s g)))
    ((pause st g) (format "~a~a" (make-string i #\space) (pp/pause i st g)))
    (`(,st . ,s) (format "~a~a~a" (make-string i #\space) (pp/state i st) (pp/stream i s)))
    (s (format "~a~a~n" (make-string i #\space) s))))

(define (step/direction d s)
  (match s
    ((mplus s1 s2)
     (let ((choices (take-path d s1 s2)))
       (let ((s1 (step (car choices)))
             (s2 (cdr choices)))
         (cond ((not s1) s2)
               ((pair? s1)
                (cons (car s1)
                      (mplus s2 (cdr s1))))
               (else (mplus s2 s1))))))
    ((bind s g)
     (cond ((not s) #f)
           ((pair? s)
            (mplus (pause (car s) g)
                   (bind (cdr s) g)))
           (else (bind s g))))
    ((pause st g) (start/step (lambda (s) s) st g))
    (_            s)))


(define (take/path path n s)
  (cond
    ((pair? s)
     (cons (car s) (take/path path (- n 1) (cdr s))))
    ((= n 0) '())
    ((null? path) '())
    (else (take/path (cdr path) n (simplify (step/direction (car path) s))))))

;; (let ((s (simplify (query (q) (layerso q '(0 0 1) '(0 1))))))
;;   (list
;;    (take/path '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 s)
;;    (walk* initial-var (state-sub (car (take/path '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1 s))))))

(define (step/choose choose s)
  (match s
    ((mplus s1 s2)
     (let* ((meatbag-data (pp/stream 2 s))
            (choice (choose meatbag-data))
            (choices (if (= choice 0)
                         `(,s1 . ,s2)
                         `(,s2 . ,s1))))
       (printf "Expanding~n~a" (pp/stream 2 (car choices)))
       (let ((s1 (step/choose choose (car choices)))
             (s2 (cdr choices)))
         (cond ((not s1) s2)
               ((pair? s1)
                (cons (car s1)
                      (mplus s2 (cdr s1))))
               (else (mplus s2 s1))))))
    ((bind s g)
     (let ((s (step/choose choose s)))
       (cond ((not s) #f)
             ((pair? s)
              (mplus (pause (car s) g)
                     (bind (cdr s) g)))
             (else (bind s g)))))
    ((pause st g) (start/step (lambda (s) (step/choose choose s)) st g))
    (_            s)))

(define (parse-integer bs)
  (let loop ((r 0)
             (i 0))
    (if (= i 4)
        r
        (loop (+ (arithmetic-shift r 8) (bytes-ref bs i))
              (+ i 1)))))

;; (define (make-choose socket)
;;   (lambda (s)
;;     (let ((out (open-output-string)))
;;       (display s out)
;;       (zmq-send socket (string->bytes/utf-8 (get-output-string out)))
;;       (let ((received (parse-integer (string->bytes/utf-8 (zmq-recv-string requester)))))
;;         (printf "received ~s~n" received)
;;         received))))

;; (define requester (zmq-socket 'pair #:connect "tcp://127.0.0.5:5555"))

;; (let ((s (simplify (query (a b) (appendo a b '(1 2 3 4)))))
;;       (choose (make-choose requester)))
;;   (list
;;    s
;;    (step/choose choose (step/choose choose s))))

(define (take/choice choose n s)
  (cond
    ((pair? s)
     (begin
       (display (format "Success:~n~a~n" (reify/initial-var (car s))))
       (cons (car s) (take/choice choose (- n 1) (cdr s)))))
    ((= n 0) '())
    ((null? s) '())
    (else (take/choice choose n (step/choose choose s)))))

;; (let ((s (simplify (query (a b) (appendo a b '(1 2 3 4)))))
;;       (choose (make-choose requester)))
;;   (take/choice choose 8 s))

(define (make-take/nas sock)
  (define (take/nas choose n s)
    (cond
      ((pair? s)
       (begin
         (zmq-send
          sock
          (string->bytes/utf-8 (format "Success: ~a~n" (reify/initial-var (car s)))))
         (cons (car s) (take/nas choose (- n 1) (cdr s)))))
      ((= n 0) '())
      ((null? s) '())
      (else (take/choice choose n (step/choose choose s)))))
  take/nas)

;; (let* ((s (simplify (query (a b) (appendo a b '(1 2 3 4)))))
;;        (take/nas (make-take/nas requester))
;;        (choose (make-choose requester)))
;;   (take/nas choose 8 s))

(define (nas-server sock)
  (define (take/path path n s)
    (cond
      ((pair? s)
       (zmq-send
        sock
        (string->bytes/utf-8 (format "Success: ~a~nStream:~n~a~n~nWhich path do you want to take? [0, 1]~n"
                                     (reify/initial-var (car s))
                                     (string->bytes/utf-8 (pp/stream 0 (cdr s)))))))
      ((or (null? s) (= n 0))
       (zmq-send sock (string->bytes/utf-8 "terminated~n")))
      (else
       (zmq-send
        sock
        (string->bytes/utf-8 (format "Stream:~n~a~n~nWhich path do you want to take? [0, 1]~n"
                                     (string->bytes/utf-8 (pp/stream 0 s)))))))
    (cond
      ((pair? s)
       (let ((received (parse-integer (string->bytes/utf-8 (zmq-recv-string sock)))))
         (cons (car s) (let ((s (simplify (step/direction received s))))
                         (take/path path n s)))))
      ((= n 0) '())
      ((null? s) '())
      (else
       (let ((received (parse-integer (string->bytes/utf-8 (zmq-recv-string sock)))))
         (let ((s (simplify (step/direction received s))))
           (take/path path n s))))))
  take/path)

(define (a-start st g)
  (match g
    ((disj g1 g2)
     (a-step (mplus (pause st g1)
                    (pause st g2))))
    ((conj g1 g2)
     (a-step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

(define (a-step s)
  (match s
    ((mplus s1 s2)
     (cond ((not s1) s2)
           ((pair? s1)
            (cons (car s1)
                  (mplus (cdr s1) s2)))
           (else (mplus s1 s2))))
    ((bind s g)
     (cond ((not s) #f)
           ((pair? s)
            (a-step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
           (else (bind s g))))
    ((pause st g) (a-start st g))
    (_            s)))

(define (mustart st g)
  (match g
    ((disj g1 g2)
     (mustep (mplus (pause st g1)
                    (pause st g2))))
    ((conj g1 g2)
     (mustep (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

;; start's pauses and propagates up mature streams
(define (mustep s)
  (match s
    ((mplus s1 s2)
     (cond ((not s1) s2)
           ((pair? s1)
            (cons (car s1)
                  (mplus (cdr s1) s2)))
           (else (mplus s1 s2))))
    ((bind s g)
     (cond ((not s) #f)
           ((pair? s)
            (mplus (pause (car s) g)
                   (bind (cdr s) g)))
           (else (bind s g))))
    ((pause st g) (mustart st g))
    (_ s)))


(define (pullup-mature-streams s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (pullup-mature-streams s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus (cdr s1) s2)))
             (else (mplus s1 s2)))))
    ((bind s g)
     (let ((s (if (mature? s) s (pullup-mature-streams s))))
       (cond ((not s) #f)
             ((pair? s)
              (mplus (pause (car s) g)
                     (bind (cdr s) g)))
             (else (bind s g)))))
    ((pause st g) (mustart st g))
    (_ s)))

(define (step-left s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (a-step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus (cdr s1) s2)))
             (else (mplus s1 s2)))))
    ((bind s g)
     (let ((s (if (mature? s) s (a-step s))))
       (cond ((not s) #f)
             ((pair? s)
              (mplus (pause (car s) g)
                     (bind (cdr s) g)))
             (else (bind s g)))))
    ((pause st g) (a-start st g))
    (_            s)))

(define (step-right s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s2) s2 (a-step s2))))
       (cond ((not s2) s1)
             ((pair? s2)
              (cons (car s2)
                    (mplus s1 (cdr s2))))
             (else (mplus s1 s2)))))
    ((bind s g)
     (let ((s (if (mature? s) s (a-step s))))
       (cond ((not s) #f)
             ((pair? s)
              (mplus (pause (car s) g)
                     (bind (cdr s) g)))
             (else (bind s g)))))
    ((pause st g) (a-start st g))
    (_            s)))

(define (final-step s)
  (match s
    ((mplus s1 s2)
     (raise (format "Didn't expect mplus in final step.~n~a~n" s)))
    ((bind s g)
     (let ((s (if (mature? s) s (a-step s))))
       (cond ((not s) #f)
             ((pair? s)
              (mplus (pause (car s) g)
                     (bind (cdr s) g)))
             (else (bind s g)))))
    ((pause st g) (a-start st g))
    (`(,st . ,s) s)
    (_           s)))

(define (step/path path s)
  (match s
    ((mplus s1 s2)
     (if (= 0 (car path))
         (let ((s1 (if (mature? s1) s1 (step/path (cdr path) s1))))
           (cond ((not s1) s2)
                 ((pair? s1)
                  (cons (car s1)
                        (mplus (cdr s1) s2)))
                 (else (mplus s1 s2))))
         (let ((s2 (if (mature? s2) s2 (step/path (cdr path) s2))))
           (cond ((not s2) s1)
                 ((pair? s2)
                  (cons (car s2)
                        (mplus s1 (cdr s2))))
                 (else (mplus s1 (cdr s2)))))))
    ((bind s g)
     (let ((s (if (mature? s) s (step/path (cdr path) s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    ((pause st g) (start st g))
    (_ s)))

;; (define test-threa
;;   (thread
;;    (lambda ()
;;      (with-output-to-file "/tmp/foo"
;;        (lambda ()
;;          (display (format "Hello, ~a~n" "World!")))
;;        #:exists 'truncate/replace))))


;; (let* ((s (simplify (query (a b) (appendo a b '(1 2 3 4)))))
;;        (take/nas (make-take/nas requester))
;;        (choose (make-choose requester)))
;;   (take/nas choose 8 s))

(define (ask sock s)
  (zmq-send sock (string->bytes/utf-8 (format "Stream:~n~a~n~nWhich path do you want to take? [0, 1]~n"
                                              (string->bytes/utf-8 (pp/stream 0 s))))))
(define (rcv sock)
  (parse-integer (string->bytes/utf-8 (zmq-recv-string sock))))

(define (turn d s1 s2)
  (if (= 0 d)
      `(,s1 . ,s2)
      `(,s2 . ,s1)))

(define (omature? s) (or (not s) (pair? s)))
(define (omature step s)
  (if (mature? s) s (mature (step s))))

(define (ostart step st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

(define (make-ostep sock)
  (define (ostep s)
    (format "~a~n" (pp/stream 0 s))
    (flush-output (current-output-port))
    (match s
      ((mplus s1 s2)
       (ask sock s)
       (let ((direction (rcv sock)))
         (if (= direction 0)
             (let ((s1 (if (mature? s1) s1 (ostep s1))))
               (cond ((not s1) s2)
                     ((pair? s1)
                      (cons (car s1)
                            (mplus (cdr s1) s2)))
                     (else (mplus s1 s2))))
             (let ((s2 (if (mature? s2) s2 (ostep s2))))
               (cond ((not s2) s1)
                     ((pair? s2)
                      (cons (car s2)
                            (mplus s1 (cdr s2))))
                     (else (mplus s1 s2)))))))
      ((bind s g)
       (let ((s (if (mature? s) s (ostep s))))
         (cond ((not s) #f)
               ((pair? s)
                (ostep (mplus (pause (car s) g)
                              (bind (cdr s) g))))
               (else (bind s g)))))
      ((pause st g) (ostart ostep st g))
      (_            s)))
  ostep)

(define output (open-output-string))

(define (exc-handler exc)
  (displayln (format "Exception: ~a~n" (raise-user-error exc))))

(define (responder-thread sock)
  (thread
   (lambda ()
     (with-handlers ([exn:fail? exc-handler])
       (with-output-to-file "/tmp/foo"
         (lambda ()
           (zmq-bind sock "tcp://127.0.0.5:5555")
           (display (format "bound~n") output)
           (let* ((s (query (a b) (appendo a b '(1 2 3 4))))
                  (ostep (make-ostep sock))
                  (step (lambda (s) (simplify (ostep s)))))
             (stream-take/step step 8 s)))
         #:exists 'truncate/replace)
       (error "An error occurred in the thread")))))

(define sock (zmq-socket 'pair))
(zmq-bind sock "tcp://127.0.0.5:5555")
;; (define t (responder-thread sock))

;;(begin (zmq-close sock) (kill-thread t))
(define (send sock msg)
  (zmq-send sock (string->bytes/utf-8 (format "~a~n"
                                              (string->bytes/utf-8 msg)))))

(define (ostream-take/step step n s)
  (if (eqv? 0 n) '()
      (let ((s (mature/step step s)))
        (if (pair? s)
            (begin (send sock (format "Solution: ~a~n" (reify/initial-var (car s))))
                   (cons (car s) (ostream-take/step step (and n (- n 1)) (cdr s))))
            '()))))

(define (orun)
  (display (format "bound~n") output)
  (let* ((s (query (a b) (appendo a b '(1 2 3 4))))
         (ostep (make-ostep sock))
         (step (lambda (s) (simplify (ostep s)))))
    (ostream-take/step step 8 s)))

(map reify/initial-var (orun))


(let ((q (query (a b) (appendo a b '(1 2 3 4)))))
  q)

;; What does this equal?
(append '(1 2) '(3 4))

;; What must "a" equal for the following to be true?
(append     a  '(3 4)) == '(1 2 3 4)

;; What must "a" and "b" equal for the following to be true?
(append     a       b) == '(1 2 3 4)





(run* (q)   (appendo '(1 2) '(3 4) q))

(run* (a)   (appendo      a '(3 4) '(1 2 3 4)))

(run* (a b) (appendo      a      b '(1 2 3 4)))
