#lang racket

(require
 "common.rkt"
 "mk-syntax.rkt"
 "strings.rkt"
 "tools.rkt"
 "utils.rkt"
 "microk-fo.rkt"
 json
 (prefix-in rm: racket/match))

(define (list->hash hsh lst)
  (cond
    [(null? lst) hsh]
    [else (list->hash (hash-set hsh (caar lst) (cdar lst)) (cdr lst))]))

(define (car-t xs) (if (null? xs) '() (car xs)))

(run 10 (fn arg out) (eval-expo `(app ,fn ,arg) '() out))

;; What follows is some utilities to help us generate training examples.
;; Why do we need this?
;; Why can't we just let the model explore on its own?
;; Because some problems don't yield a success/result for a long time.
;; It's better to have some positive examples to bootstrap a reasonable
;; search policy.

;; policy-print, policy-read, policy-done?
(define (gen-train-example-write-policy exp-loc qvars)
  (let ([s exp-loc])
    (let ([choices (explore-node-choices (explore-loc-tree s))])
      (define results (takef choices state?))
      (when (not (null? results))  ; Only print when there are results
        (define chs (dropf choices state?))
        (define (serialize-state st)
          (let* ([walked-vars (walked-term initial-var st)])
            (map cons qvars walked-vars)))
        (define (serialize-result results)
          (serialize-state results))
        (define (serialize-choice choice)
          (rm:match choice
                    [(pause st g) `((state ,(serialize-state st)) (goal ,(~a (pretty/goal st g))))]
                    [_ (serialize-state choice)]))
        (printf "~a\n"
                (jsexpr->string
                 (hash 'results
                       (list->hash (hash) (car-t (map serialize-result results)))
                       'choices
                       (map (compose ~a serialize-choice) chs))))))))

(define (gen-train-example-read-policy exp-loc)
  (let* ([tree (explore-loc-tree exp-loc)]
         [choices (explore-node-choices tree)]
         [num-choices (length choices)])
    (if (> num-choices 0)
        (add1 (random num-choices))  ; add1 because choices are 1-based
        'u)))  ; return undo if no choices available

(define (gen-train-example-finished? exp-loc)
  (let* ([tree (explore-loc-tree exp-loc)]
         [choices (explore-node-choices tree)]
         [finished-index (index-where
                           choices
                           (lambda (x) (state? x)))]
        [valid-index (exact-nonnegative-integer? finished-index)])
    valid-index))

(define (drive/policy step qvars policy-print policy-read policy-done? init-state)
  (let loop ([s init-state])
    (unless (policy-done? s)
      (let* ([input (policy-read s)]  ; Pass the current state to policy-read
             [tree (explore-loc-tree s)])
        (policy-print s qvars)  ; Print state before making choice
        (loop
          (cond
            [(and (integer? input) (<= 1 input) (<= input (length (explore-node-choices tree))))
             (explore-choice s step (- input 1))]
            [(or (eq? input 'u) (eq? input 'undo)) (explore-undo s)]
            [else s])))
      s)))

(define-syntax drive/training-examples
  (syntax-rules (query)
    [(_ step (query (qvars ...) body ...))
     (drive/policy
      step
      '(qvars ...)
      gen-train-example-write-policy
      gen-train-example-read-policy
      gen-train-example-finished?
      (init-explore (query (qvars ...) body ...)))]))


(comment
 (drive/training-examples
  step
  (query (fn arg out)
         (eval-expo `(app ,fn ,arg) '() out)))

 )

;; Collect training examples with full path information
(define (collect-training-examples step query-form qvars max-examples)
  (let* ([init-state (init-explore query-form)]
         [examples '()]
         [path-history '()])
    
    ;; Helper to check if we have a result and record it
    (define (check-for-results s)
      (let* ([tree (explore-loc-tree s)]
             [choices (explore-node-choices tree)]
             [results (takef choices state?)])
        (when (not (null? results))
          (set! examples (cons s examples)))))
    
    ;; Modified policy functions
    (define (policy-print s _) 
      (check-for-results s)
      (void))
    
    (define (policy-read s)
      (let* ([tree (explore-loc-tree s)]
             [choices (explore-node-choices tree)]
             [num-choices (length choices)])
        (if (> num-choices 0)
            (add1 (random num-choices))
            'u)))
    
    (define (policy-done? _)
      (>= (length examples) max-examples))
    
    ;; Run the search
    (drive/policy step qvars policy-print policy-read policy-done? init-state)
    
    ;; Return collected examples
    examples))

; Usage example:
(comment
 (define examples
   (collect-training-examples
    step
    (query (fn arg out) (eval-expo `(app ,fn ,arg) '() out))
    '(fn arg out)
    1))

 (extract-path (car examples))

 )
;; You can reconstruct the complete path if needed using the explore-loc structure
(define (extract-path loc)
  (let loop ((current-loc loc)
             (path '()))
    (match current-loc
      [(explore-loc _ 'X-TOP) (reverse path)]
      [(explore-loc _ (explore-context index _ _ parent))
       (loop (explore-undo current-loc) (cons index path))])))
