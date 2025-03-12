#lang racket/base

(require
  racket/list
  racket/set
  racket/string
  racket/random
  racket/pretty
  racket/format
  racket/async-channel
  (prefix-in rm: racket/match)
  racket/serialize
  json
  zeromq
  first-order-miniKanren/microk-fo
  first-order-miniKanren/tools
  first-order-miniKanren/math)

(define-relation (conso head tail result)
   (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
  (conde
   ((== '() ab) (== c abc))
   ((fresh (a b bc)
           (conso a b ab)
           (conso a bc abc)
           (appendo b c bc)))))

(define (serialize choices qvars)
  (define (serialize-state st)
    (let* ([walked-vars (walked-term initial-var st)])
      `(state
        (vars ,@(map cons qvars walked-vars)))))
  (define (serialize-choice choice)
    (rm:match choice
      [(pause st g)
       `(choice
         ,(serialize-state st)
         (goal ,(pretty/goal st g)))]
      [_ (serialize-state choice)]))
  (let ([results (takef choices state?)]
        [chs (dropf choices state?)])
    `(search-state
      (results ,@(map serialize-choice results))
      (choices ,@(map serialize-choice chs)))))


(define (serialize-to-json choices qvars)
  (define (serialize-state st)
    (let* ([walked-vars (walked-term initial-var st)])
      (map cons qvars walked-vars)))
  (define (serialize-choice choice)
    (rm:match choice
      [(pause st g)
       (hash 'state (serialize-state st)
             'goal (pretty/goal st g))]
      [_ (serialize-state choice)]))
  (define results (takef choices state?))
  (define chs (dropf choices state?))
  (hash 'results (map serialize-choice results)
        'choices (map serialize-choice chs)))


(let* ((q "(query (a b) (appendo a b '(1 2 3 4)))")
       (q (read (open-input-string q)))
       (qvars (cadr q))
       (s (init-explore (eval q (current-namespace)))))
  (let* ((s (explore-choice s step 0))
         (s (explore-choice s step 1))
         (s (explore-choice s step 0)))
    (serialize-to-json (explore-node-choices (explore-loc-tree s)) qvars)))

(let ((s (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
  (let* ((s (explore-choice s step 0))
         (s (explore-choice s step 1))
         (s (explore-choice s step 0)))
    (serialize-to-json (explore-node-choices (explore-loc-tree s)) '(a b))))

(let ((s (init-explore
          (query (p)
                 (fresh (body)
                        (== p `(lambda ,body))
                        (eval-expo `(app ,p ,(make-num 3)) '() (make-num 9))
                        (eval-expo `(app ,p ,(make-num 4)) '() (make-num 16)))))))
  (let* ((s (explore-choice s step 0))
         (s (explore-choice s step 0))
         (s (explore-choice s step 0)))
    (serialize-to-json (explore-node-choices (explore-loc-tree s)) '(p))))

;; Socket setup and choice getter
(define (make-connection)
  (define socket (zmq-socket 'pair))
  (zmq-connect socket "tcp://127.0.0.1:5555")
  socket)

(define connection-socket (make-connection))

(define (get-python-choice choices qvars)
  (zmq-send connection-socket
        (string->bytes/utf-8
         (format "~s" (serialize choices qvars))))

  (define response-bytes (zmq-recv connection-socket))
  (define response-str (bytes->string/utf-8 response-bytes))
  (define choice-index (string->number response-str))
  choice-index)


;; (let ((c (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
;;   (let loop ((s (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
;;     (unless (explore-tree-finished? s)
;;       (print s)
;;       (let ([input (get-python-choice (explore-node-choices (explore-loc-tree s)) '(qvars (a b)))]
;;             [tree (explore-loc-tree s)])
;;         (print input)
;;         (print (integer? input))
;;         (loop
;;          (cond
;;            [(and (integer? input) (<= 1 input) (<= input (length (explore-node-choices tree))))
;;             (explore-choice s step input)]
;;            [(or (eq? input 'u) (eq? input 'undo)) (explore-undo s)]
;;            [else s])))
;;       (takef (explore-node-choices (explore-loc-tree s)) state?))))

;; (let ((s (init-explore (query (p) (eval-expo p '() '())))))
;;   (let loop ((s (explore-choice s step 0)))
;;     (define results (takef (explore-node-choices (explore-loc-tree s)) state?))
;;     (define choices (dropf (explore-node-choices (explore-loc-tree s)) state?))
;;     ;(printf "Results: ~a~n" results)
;;     (cond
;;       ((<= 1 (length results))
;;        results)
;;       ((< 0 (length choices))
;;        (let* ((choices (explore-node-choices (explore-loc-tree s)))
;;               (i (random (length choices))))
;;          ;(printf "Choosing ~a~n" i)
;;          ;(display (serialize-choices (explore-node-choices (explore-loc-tree s)) '(p)))
;;          ;(printf "Choosing index ~a~n" i)
;;          (loop (explore-choice s step (random (length (explore-node-choices (explore-loc-tree s))))))))
;;       (else
;;        (loop (explore-undo s))))))

;; (let ((c (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
;;   (let loop ((s (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
;;     (unless (explore-tree-finished? s)
;;       (print s)
;;       (let ([input (get-python-choice (explore-node-choices (explore-loc-tree s)) '(qvars (a b)))]
;;             [tree (explore-loc-tree s)])
;;         (print input)
;;         (print (integer? input))
;;         (loop
;;          (cond
;;            [(and (integer? input) (<= 1 input) (<= input (length (explore-node-choices tree))))
;;             (explore-choice s step input)]
;;            [(or (eq? input 'u) (eq? input 'undo)) (explore-undo s)]
;;            [else s])))
;;       (takef (explore-node-choices (explore-loc-tree s)) state?))))

(zmq-close connection-socket)

;; Example usage:
#|
(define (example-search)
  (let* ([query (query (a b) (appendo a b '(1 2 3 4)))]
         [init-state (init-explore query)]
         [python-driver (drive/python step '(a b))])
    (let loop ([state init-state])
      (if (explore-tree-finished? state)
          (begin
            (cleanup-sockets)
            state)
          (loop (python-driver state))))))
|#

;; policy-print, policy-read, policy-done?
;; (define (make-serialize-tree queue)
;;   (lambda (serialize-tree exp-loc qvars)
;;     (let ((tree (explore-loc-tree exp-loc)))
;;       (enqueue queue (explore-node-choices tree) qvars))))

;; (define-syntax drive/queue
;;   (syntax-rules (query)
;;     [(_ step (query (qvars ...) body ...))
;;      (drive/policy
;;       step
;;       '(qvars ...)
;;       pp/)]))

;; Learn to balanced append.

;; Learn to count outwards from the midpoint.


;; Define a backend protocol with just two operations:
;; - send a message to the backend
;; - receive a choice from the backend
(struct backend (send receive))

;; Python socket backend implementation
(define (make-python-backend)
  (define socket (zmq-socket 'pair))
  (zmq-connect socket "tcp://localhost:5555")

  (backend
   ;; Send serialized choices to Python
   (lambda (choices qvars)
     (zmq-send socket
               (string->bytes/utf-8
                (format "~s" (serialize choices qvars)))))

   ;; Receive choice index from Python
   (lambda ()
     (define response-bytes (zmq-recv socket))
     (define response-str (bytes->string/utf-8 response-bytes))
     (string->number response-str))))

;; Queue-based backend for testing
(define (make-queue-backend)
  (define input-channel (make-async-channel))
  (define output-channel (make-async-channel))

  ;; Expose the channels to allow interaction from REPL
  (set! *input-channel* input-channel)
  (set! *output-channel* output-channel)

  (backend
   ;; Send serialized choices to queue
   (lambda (choices qvars)
     (async-channel-put
      output-channel
      (list choices qvars (serialize choices qvars))))

   ;; Receive choice index from queue
   (lambda ()
     (async-channel-get input-channel))))

;; Global channels for REPL interaction
(define *input-channel* #f)
(define *output-channel* #f)

;; Global variable to hold the current backend
(define current-backend (make-queue-backend))

;; Function to switch backends
(define (use-python-backend!)
  (set! current-backend (make-python-backend)))

(define (use-queue-backend!)
  (set! current-backend (make-queue-backend)))

;; Updated get-choice function using the current backend
(define (get-backend-choice choices qvars)
  ((backend-send current-backend) choices qvars)
  (define choice-index ((backend-receive current-backend)))
  (list-ref (dropf choices state?) choice-index))

;; Helper function for REPL interaction - read from queue
(define (read-next-choices)
  (if *output-channel*
      (async-channel-try-get *output-channel*)
      (error "Queue backend not active")))

;; Helper function for REPL interaction - respond with choice
(define (select-choice index)
  (if *input-channel*
      (async-channel-put *input-channel* index)
      (error "Queue backend not active")))

(let ((s (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
  (list s (explore-choice s step 0)))

