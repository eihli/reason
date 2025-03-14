#lang racket/base

(require
  racket/list
  racket/format
  (prefix-in rm: racket/match)
  json
  zeromq
  first-order-miniKanren/microk-fo
  first-order-miniKanren/tools)

(define-syntax-rule (comment body ...)
  (void))

(define-relation (conso head tail result)
   (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
  (conde
   ((== '() ab) (== c abc))
   ((fresh (a b bc)
           (conso a b ab)
           (conso a bc abc)
           (appendo b c bc)))))

(define (list->hash hsh lst)
  (cond
    ((null? lst) hsh)
    (else (list->hash (hash-set hsh (caar lst) (cdar lst)) (cdr lst)))))

(comment
 (list->hash (hash) '((a 1) (b 2 3 4) (c 5 6)))

 )

(define (car-t xs)
  (if (null? xs) '() (car xs)))

(define (serialize-to-json s qvars)
  (define (serialize-state st)
    (let* ([walked-vars (walked-term initial-var st)])
      (map cons qvars walked-vars)))
  (define (serialize-result results)
    (serialize-state results))
  (define (serialize-choice choice)
    (rm:match choice
              [(pause st g)
               `((state ,(serialize-state st))
                 (goal ,(pretty/goal st g)))]
              [_ (serialize-state choice)]))
  (let ((choices (explore-node-choices (explore-loc-tree s))))
    (define results (takef choices state?))
    (define chs (dropf choices state?))
    (jsexpr->string (hash 'results (list->hash (hash) (car-t (map serialize-result results)))
                          'choices (map (compose ~a serialize-choice) chs)))))

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
 (let* ((q "(query (a b) (appendo a b '(1 2 3 4)))")
        (qvars (cadr (read (open-input-string q))))
        (q (eval-with-query q))
        (s (init-explore (eval (expand q) (current-namespace)))))
   (let* ((s (explore-choice s step 0))
          (s (explore-choice s step 0))
          (s (explore-choice s step 0)))
     (serialize-to-json s qvars)))

;; "{\"choices\":[\"((state ((a 1 2) (b 3 4))) (goal (conj (== () ()) (== (3 4) (3 4)))))\",\"((state ((a 1 2 3 . #s(var zs 10)) (b . #s(var b 2)))) (goal (conj (conj (== (3 . #s(var zs 10)) (3 . #s(var zs 10))) (== (3 4) (3 4))) (relate (appendo #s(var zs 10) #s(var b 2) (4))))))\"],\"results\":{}}"
 )

(comment
 (let ((s (init-explore
           (query (p)
                  (fresh (body)
                         (== p `(lambda ,body))
                         (eval-expo `(app ,p ,(make-num 3)) '() (make-num 9))
                         (eval-expo `(app ,p ,(make-num 4)) '() (make-num 16)))))))
   (let* ((s (explore-choice s step 0))
          (s (explore-choice s step 0))
          (s (explore-choice s step 0)))
     (serialize-to-json s '(p))))
 )

(define shutdown-channel (make-channel))

(define (responder-thread)
  (thread
   (lambda ()
     (let ([responder (zmq-socket 'rep)])
       (zmq-set-option responder 'rcvtimeo 100)
       (zmq-bind responder "tcp://127.0.0.1:5555")
       (printf "Server started on tcp://127.0.0.1:5555~n")
       (let loop ((s '()) (qvars '()))
         (define should-shutdown?
           (sync/timeout 0 shutdown-channel))
         (if should-shutdown?
             (begin
               (printf "Shutting down server...~n")
               (zmq-close responder)
               (printf "Socket closed~n"))
             (begin
               ;; Try to receive a message with timeout
               (with-handlers ([exn:fail? (lambda (e)
                                            ;; Handle timeout or other errors
                                            (void))])
                 (let ([msg (string->jsexpr (zmq-recv-string responder))])
                   (when msg
                     (printf "Server received: ~s~n" msg)
                     (cond
                       ((hash-has-key? msg 'query)
                        (printf "Received a reset.~n")
                        (let* ((q (hash-ref msg 'query))
                               (qvars (cadr (read (open-input-string q))))
                               (q (eval-with-query q))
                               (s (init-explore (eval (expand q) (current-namespace)))))
                          (zmq-send responder (serialize-to-json s qvars))
                          (loop s qvars)))
                       ((hash-has-key? msg 'choice)
                        (printf "Received a choice: ~a~n" (hash-ref msg 'choice))
                        (let* ((choice (hash-ref msg 'choice))
                               (s (explore-choice s step choice)))
                          ;; Handle exceptions in this, since it's in a thread,
                          ;; they get swallowed otherwise.
                          (with-handlers ([exn:fail? (lambda (e)
                                                      (printf "Error processing choice: ~s~n" e)
                                                      (zmq-send responder (jsexpr->string (hash 'error (format "~a" e))))
                                                      (loop s qvars))])
                            (zmq-send responder (serialize-to-json s qvars))
                            (loop s qvars)))))))))))))))

(define server-thread (responder-thread))

;; Function to cleanly shut down the server
(define (stop-server)
  (printf "Stopping server...~n")
  (channel-put shutdown-channel 'shutdown)
  (thread-wait responder-thread)
  (printf "Server stopped~n"))

(comment
 (stop-server)

 )
