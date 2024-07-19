#lang racket/base

(provide run)

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

(define (start req)
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

(let ((choices (serialize-choices
                  (stream->choices
                   (parallel-step
                    (pause
                     empty-state
                     (fresh (a b)
                            (== `(,a ,b) initial-var)
                            (appendo a b '(1 2 3 4)))))))))
    (hash 'choices choices))

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
           (countero hidden)
           (<lo '(1) hidden)
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

(run* (a b)
  (appendo a b '(1 2 3 4)))

(define (mature/step step s)
  (if (mature? s) s (mature/step step (step s))))

(define (stream-take/step step n s)
  (if (eqv? 0 n) '()
      (let ((s (mature/step step s)))
        (if (pair? s)
            (cons (car s) (stream-take/step step (and n (- n 1)) (cdr s)))
            '()))))

(define (simplify s)
  (prune/stream (dnf/stream s)))

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

(let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
  (map reify/initial-var (stream-take/step (lambda (s) (simplify (ow-step s))) 4 (simplify stream))))

;;;; TODO
;; make your serialized choices as brief as explore's

(let ((stream (query (q) (layerso q (build-num 768) (build-num 10)))))
  (parse-nums (map reify/initial-var (stream-take/step (lambda (s) (simplify (ow-step s))) 3 (simplify stream)))))

(let ((stream (query (q) (layerso q (build-num 768) (build-num 10)))))
  (explore ow-step (simplify stream)))

(let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
 (list
  (serialize-choices (stream->choices (choices (parallel-step stream))))
  (serialize-choices (stream->choices (choices (parallel-step (parallel-step stream)))))))

;; (run/step-simplify ow-step 2 (a b) (appendo a b '(1 2 3 4)))
(let ((stream (query (a b) (appendo a b '(1 2 3 4)))))
 (list
  (serialize-choices (stream->choices (parallel-step stream)))
  (parallel-step (parallel-step (parallel-step stream)))
  (serialize-choices (stream->choices (parallel-step (parallel-step (parallel-step stream)))))))

;; Using parallel-step allows us to expand constraints from both examples
;; simultaneously, pruning impossibilities much sooner.

(require racket/trace)


(untrace ow-step)

;; lengtho so we don't run forever.
(define-relation (lengtho l n)
  (conde
   ((== l '()) (== n '()))
   ((fresh (head tail result)
      (== `(,head . ,tail) l)
      (pluso '(1) result n)
      (lengtho tail result)))))

(run* (a b)
  (appendo a b '(1 2 3 4)))

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

(with-output-to-file "/tmp/out"
  (lambda ()
    (for-each
     (lambda (arch)
       (write (car arch))
       (newline))
     (parse-nums
      (run 10 (q)
           (layerso q (build-num 768) (build-num 10))))))
  #:exists 'replace)


;; (explore parallel-step (prune/stream (dnf/stream (query (q) (layerso q '(0 1) '(0 1))))))

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
