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

;; Improved biased interleaving policy that better mimics microKanren's step
(define (biased-interleaving-policy-with-removal exp-loc)
  (let* ([tree (explore-loc-tree exp-loc)]
         [choices (explore-node-choices tree)]
         [num-choices (length choices)]
         [path (extract-path exp-loc)]
         [path-length (length path)])

    (cond
      ;; No choices available - undo
      [(= num-choices 0) 'u]

      ;; Only one choice - take it
      [(= num-choices 1) 1]

      ;; Multiple choices - implement biased interleaving
      [else
       ;; Check if the first choice is a state (result)
       (let ([first-choice (first choices)])
         (if (state? first-choice)
             ;; If first choice is a result, take it
             1
             ;; Otherwise, implement interleaving strategy
             (let ([context (explore-loc-context exp-loc)])
               (cond
                 ;; At root, start with first branch
                 [(eq? context 'X-TOP) 1]

                 ;; Implement depth bound to prevent going too deep in one branch
                 [(> path-length 5)
                  (printf "  Depth limit reached, undoing\n")
                  'u]

                 ;; Implement breadth-first exploration after certain depth
                 [(> path-length 2)
                  ;; After going a few levels deep, start exploring breadth-first
                  ;; by cycling through all available choices
                  (let* ([current-index (if (explore-context? context)
                                           (explore-context-index context)
                                           0)]
                         [next-index (remainder (add1 current-index) num-choices)])
                    (add1 next-index))]

                 ;; Otherwise use standard biased interleaving
                 [else
                  (let ([current-index (if (explore-context? context)
                                          (explore-context-index context)
                                          0)])
                    ;; If we're at the first branch, try the second branch next
                    (if (= current-index 0)
                        (if (>= num-choices 2) 2 1)
                        ;; Otherwise, continue with depth-first on current branch
                        (if (< (add1 current-index) num-choices)
                            (add1 current-index)
                            'u)))]))))])))

;; Modified explore-undo that removes the current node from parent's choices
(define (explore-undo-with-removal exp-loc)
  (match exp-loc
    [(explore-loc tree (explore-context i siblings choices ctx))
     ;; Create a new context with the current node removed from choices
     (let* ([filtered-choices (if (null? choices)
                                 '()
                                 (remove-choice choices i))])
       ;; If there are no more choices after removal, propagate the removal upward
       (if (null? filtered-choices)
           (explore-loc (explore-node i filtered-choices siblings) ctx)
           (explore-loc (explore-node i filtered-choices siblings) ctx)))]
    [(explore-loc t 'X-TOP) (explore-loc t 'X-TOP)]))

;; Helper function to remove a choice at index i
(define (remove-choice choices i)
  (append (take choices i)
          (drop choices (add1 i))))

;; Drive function that uses node removal instead of path tracking
(define (drive/biased-interleaving-with-removal step qvars init-state max-steps)
  (let ([step-count 0])

    (define (policy-print s qvars)
      (set! step-count (add1 step-count))
      (void))

    (define (policy-done? s)
      (>= step-count max-steps))

    ;; Modified loop that handles node removal
    (let loop ([s init-state])
      (policy-print s qvars)
      (unless (policy-done? s)
        (let* ([tree (explore-loc-tree s)]
               [choices (explore-node-choices tree)]
               [policy-choice (biased-interleaving-policy-with-removal s)])

          (loop
            (cond
              ;; If we have a valid choice, explore it
              [(and (integer? policy-choice)
                    (<= 1 policy-choice)
                    (<= policy-choice (length choices)))
               (explore-choice s step (- policy-choice 1))]

              ;; If we need to undo, use our modified undo function
              [(eq? policy-choice 'u)
               (explore-undo-with-removal s)]

              ;; Otherwise stay at current node
              [else s]))))
      s)))

;; Debug version with improved printing
(define (drive/biased-interleaving-with-removal-debug step qvars init-state max-steps)
  (let ([step-count 0]
        [results-found 0])

    (define (policy-print s qvars)
      (let* ([tree (explore-loc-tree s)]
             [choices (explore-node-choices tree)]
             [results (takef choices state?)])

        (printf "Step ~a: ~a choices available (~a results)\n"
                step-count
                (length choices)
                (length results))

        ;; Print any results found
        (when (and (not (null? results)) (= results-found 0))
          (printf "  RESULT FOUND!\n")
          (set! results-found (add1 results-found)))

        (set! step-count (add1 step-count)))
      (void))

    ;; Modified loop that handles node removal
    (let loop ([s init-state])
      (policy-print s qvars)
      (unless (or (>= step-count max-steps) (> results-found 0))
        (let* ([tree (explore-loc-tree s)]
               [choices (explore-node-choices tree)]
               [policy-choice (biased-interleaving-policy-with-removal s)])

          (printf "  Path: ~a, Choice: ~a\n" (extract-path s) policy-choice)

          (loop
            (cond
              ;; If we have a valid choice, explore it
              [(and (integer? policy-choice)
                    (<= 1 policy-choice)
                    (<= policy-choice (length choices)))
               (explore-choice s step (- policy-choice 1))]

              ;; If we need to undo, use our modified undo function that removes the node
              [(eq? policy-choice 'u)
               (printf "  Undoing and removing node\n")
               (explore-undo-with-removal s)]

              ;; Otherwise stay at current node
              [else s]))))
      s)))

;; Collect examples using the improved approach
(define (collect-examples/biased-interleaving-with-removal step query-form qvars max-examples max-steps)
  (let* ([init-state (init-explore query-form)]
         [examples '()]
         [count 0]
         [step-count 0])

    ;; Helper to check if we have a result and record it
    (define (check-for-results s)
      (let* ([tree (explore-loc-tree s)]
             [choices (explore-node-choices tree)]
             [results (takef choices state?)])
        (when (not (null? results))
          (set! examples (cons s examples))
          (set! count (add1 count)))))

    ;; Modified loop that handles node removal
    (let loop ([s init-state])
      (check-for-results s)
      (set! step-count (add1 step-count))

      (unless (or (>= count max-examples) (>= step-count max-steps))
        (let* ([tree (explore-loc-tree s)]
               [choices (explore-node-choices tree)]
               [policy-choice (biased-interleaving-policy-with-removal s)])

          (loop
            (cond
              ;; If we have a valid choice, explore it
              [(and (integer? policy-choice)
                    (<= 1 policy-choice)
                    (<= policy-choice (length choices)))
               (explore-choice s step (- policy-choice 1))]

              ;; If we need to undo, use our modified undo function
              [(eq? policy-choice 'u)
               (explore-undo-with-removal s)]

              ;; Otherwise stay at current node
              [else s]))))

      examples)))

;; Usage example
(comment
 (define examples
   (collect-examples/biased-interleaving-with-removal
    step
    (query (fn arg out) (eval-expo `(app ,fn ,arg) '() out))
    '(fn arg out)
    1
    1000))

 (extract-path (car examples))

 (drive/biased-interleaving-with-removal-debug
  step
  '(fn arg out)
  (init-explore (query (fn arg out) (eval-expo `(app ,fn ,arg) '() out)))
  100)

 )
