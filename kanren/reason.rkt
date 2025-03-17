#lang racket/base

(require racket/list
         racket/format
         racket/struct
         (prefix-in rm: racket/match)
         json
         net/zmq
         first-order-miniKanren/microk-fo
         first-order-miniKanren/tools
         first-order-miniKanren/math)

(define-syntax-rule (comment body ...)
  (void))

(define-relation (conso head tail result) (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
                 (conde ((== '() ab) (== c abc))
                        ((fresh (a b bc) (conso a b ab) (conso a bc abc) (appendo b c bc)))))

(define (list->hash hsh lst)
  (cond
    [(null? lst) hsh]
    [else (list->hash (hash-set hsh (caar lst) (cdar lst)) (cdr lst))]))

(comment (list->hash (hash) '((a 1) (b 2 3 4) (c 5 6))))

(define (car-t xs) (if (null? xs) '() (car xs)))

(define (serialize-to-json s qvars)
  (define (serialize-state st)
    (let* ([walked-vars (walked-term initial-var st)]) (map cons qvars walked-vars)))
  (define (serialize-result results)
    (serialize-state results))
  (define (serialize-choice choice)
    (rm:match choice
              [(pause st g) `((state ,(serialize-state st)) (goal ,(~a (pretty/goal st g))))]
              [_ (serialize-state choice)]))
  (let ([choices (explore-node-choices (explore-loc-tree s))])
    (define results (takef choices state?))
    (define chs (dropf choices state?))
    (jsexpr->string (hash 'results
                          (list->hash (hash) (car-t (map serialize-result results)))
                          'choices
                          (map (compose ~a serialize-choice) chs)))))

(comment
 (let ((goal (query (a b) (appendo a b '(1 2 3 4)))))
   (~a (pretty/goal (pause-pause-state goal) (pause-pause-goal goal))))

 )


;; A helper function to evaluate user-supplied input containing macros:
(define (eval-with-query str)
  (define ns (make-base-namespace))
  ;; Bring the macro & anything else needed into that namespace.
  (parameterize ([current-namespace ns])
    (namespace-require 'racket)
    (namespace-require 'first-order-miniKanren/math)
    (namespace-require 'first-order-miniKanren/microk-fo)
    (namespace-require 'first-order-miniKanren/tools)
    ;; Now 'query' is available at compile-time (phase 1) within 'ns'.
    ;; Parse the user input from a string:
    (define user-expr (read (open-input-string str)))
    (eval user-expr)))

(comment
 (let* ([q "(query (a b) (appendo a b '(1 2 3 4)))"]
        [qvars (cadr (read (open-input-string q)))]
        [q (eval-with-query q)]
        [s (init-explore (eval (expand q) (current-namespace)))])
   (let* ([s (explore-choice s step 0)]
          [s (explore-choice s step 1)]
          [s (explore-choice s step 1)])
     (serialize-to-json s qvars))

   (let* ([q "
   (query (p)
          (fresh (body)
                 (== p `(lambda ,body))
                 (eval-expo `(app ,p ,(make-num 3)) '() (make-num 9))
                 (eval-expo `(app ,p ,(make-num 4)) '() (make-num 16))))
"]
          [qvars (cadr (read (open-input-string q)))]
          [q (eval-with-query q)]
          [s (init-explore (eval (expand q) (current-namespace)))])
     (let* ([s (explore-choice s step 0)]
            [s (explore-choice s step 0)]
            [s (explore-choice s step 0)])
       (serialize-to-json s qvars)))

   )
 ;; "{\"choices\":[\"((state ((a 1 2) (b 3 4))) (goal (conj (== () ()) (== (3 4) (3 4)))))\",\"((state ((a 1 2 3 . #s(var zs 10)) (b . #s(var b 2)))) (goal (conj (conj (== (3 . #s(var zs 10)) (3 . #s(var zs 10))) (== (3 4) (3 4))) (relate (appendo #s(var zs 10) #s(var b 2) (4))))))\"],\"results\":{}}"
 )

(comment
 (let ([s (init-explore (query (p)
                               (fresh (body)
                                      (== p `(lambda ,body))
                                      (eval-expo `(app ,p ,(make-num 3)) '() (make-num 9))
                                      (eval-expo `(app ,p ,(make-num 4)) '() (make-num 16)))))])
   (let* ([s (explore-choice s step 0)]
          [s (explore-choice s step 0)]
          [s (explore-choice s step 0)])
     (serialize-to-json s '(p))))

 )

(define (shutdown ctx sock)
  (printf "Shutting down server...~n")
  (set-socket-option! sock 'LINGER 1000)
  (socket-close! sock)
  (context-close! ctx)
  (printf "Socket closed~n"))

(define (serve ctx)
  (define sock (socket ctx 'REP))
  (define poll-items (make-vector 1 (make-poll-item sock 0 '(POLLIN) '())))
  (socket-bind! sock "tcp://127.0.0.1:5555")
  (let loop ([s '()]
             [qvars '()])
    (let ([rc (poll! poll-items 1000)])
      (if rc
          (let ([msg (socket-recv! sock)])
            (when (and msg (not (zero? (bytes-length msg))))
              (let ([msg (string->jsexpr (bytes->string/utf-8 msg))])
                (cond
                  [(hash-has-key? msg 'query)
                   (printf "Received a reset.~n")
                   (let* ([q (hash-ref msg 'query)]
                          [qvars (cadr (read (open-input-string q)))]
                          [q (eval-with-query q)]
                          [s (init-explore q)])
                     ;; Send the response
                     (define response (string->bytes/utf-8 (serialize-to-json s qvars)))
                     (socket-send! sock response)
                     (loop s qvars))]
                  [(hash-has-key? msg 'choice)
                   (printf "Received a choice: ~a~n" (hash-ref msg 'choice))
                   (let* ([choice (hash-ref msg 'choice)]
                          [s (explore-choice s step choice)])
                     (with-handlers ([exn:fail? (lambda (e)
                                                  (printf "Error processing choice: ~s~n" e)
                                                  (socket-send! sock
                                                                (string->bytes/utf-8
                                                                 (jsexpr->string
                                                                  (hash 'error
                                                                        (format "~a" e)))))
                                                  (loop s qvars))])
                       (socket-send! sock (string->bytes/utf-8 (serialize-to-json s qvars)))
                       (loop s qvars)))]))))
          (loop s qvars)))))

(define (start-server)
  (define ctx (context 1))
  (serve ctx))

(comment (define-values (thr ctx) (start-server-1)))

(define (stop-server ctx sock)
  (printf "Stopping server...~n")
  (shutdown ctx sock))

(comment (stop-server))

(module+ main
  (printf "starting server~n")
  (start-server)
  )
