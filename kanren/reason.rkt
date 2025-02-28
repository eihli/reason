#lang racket/base

(require
  racket/list
  racket/set
  racket/string
  racket/random
  racket/format
  racket/async-channel
  (prefix-in rm: racket/match)
  racket/serialize
  zeromq
  first-order-miniKanren/microk-fo
  first-order-miniKanren/tools
  first-order-miniKanren/math)

;; Serialize choices.
;; choices might be "choices" or they might be "results".
;; Choices that are true of the `state?` predicate are "results".
;; Otherwise, they are "choices".
;; This serializes both.
;;
;; Here's an example of serialzing for a stdin-/stdout-driven policy.
;;
;; (define (pp/explore-tree exp-loc qvars)
;;   (define tree (explore-loc-tree exp-loc))
;;   #| (printf "Tree: ~s\n" tree) |#
;;   #| (printf "Context: ~s\n" (explore-loc-context exp-loc)) |#
;;   (pprint-choices (explore-node-choices tree) qvars))
;;
;; Where `pprint-choices` is defined as:
;; (define (pprint-choices choices qvars)
;;   (define chs (dropf choices state?))
;;   (define results (takef choices state?))
;;   (when (and (= 0 (length chs)) (null? results))
;;     (printf "No more choices available. Undo to continue.\n"))
;;   (unless (null? chs)
;;     (printf "Number of Choices: ~a\n" (length chs))
;;     (for-each (lambda (i s)
;;                 (printf (string-append "\nChoice ~s:\n") (+ i 1))
;;                 (pprint-choice s qvars))
;;               (range (length chs)) chs))
;;   (unless (null? results)
;;     (printf "Number of results: ~a\n" (length results))
;;     (for-each (lambda (st)
;;                 (pprint-result st qvars)
;;                 (newline))
;;               results)))
;;
;; The difference between that serialization and this serialization
;; is that this one will be sent to a Python process for machine
;; learning. Rather than human-readable, we'll want to maintain
;; as much of the tree/graph-like structure as possible, to
;; feed through something like a graph neural network.


;; Serialize choices for Python to analyze
(define (serialize choices qvars)
  (define (serialize-state st)
    (let* ([walked-vars (walked-term initial-var st)]
           [constraints (if (state? st)
                          '()
                          (walked-term
                           (goal->constraints st
                            (pause-pause-goal (cadr (filter pause? choices))))))])
      `(state
        (vars ,@(if qvars
                    (map cons qvars walked-vars)
                    walked-vars))
        (constraints ,@constraints))))

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

(define-relation (conso head tail result)
   (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
  (conde
   ((== '() ab) (== c abc))
   ((fresh (a b bc)
           (conso a b ab)
           (conso a bc abc)
           (appendo b c bc)))))

(let ((q (query (a b) (appendo a b '(1 2 3 4)))))
  (list q (stream->choices (step q))))

;; (drive/stdio step (query (a b) (appendo a b '(1 2 3 4))))

;; Socket setup and choice getter
(define (make-connection)
  (define socket (zmq-socket 'pair))
  (zmq-connect socket "tcp://127.0.0.5:5555")
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

(let ((q (query (a b) (appendo a b '(1 2 3 4)))))
  q)



(define (serialize-qvars qvars vs)
  (define (qv-prefix qv) (string-append " " (symbol->string qv) " = "))
  (define qv-prefixes (and qvars (map qv-prefix qvars)))
  (if qv-prefixes
      (string-join
       (map (lambda (prefix v) (~a prefix v))
            qv-prefixes vs)
       "\n")
      (string-join
       (map (lambda (v) (~a " " v)) vs)
       "\n")))

(define (serialize-choice s qvars)
  (rm:match s
    [(pause st g)
     (define qvars-output (serialize-qvars qvars (walked-term initial-var st)))
     (define cxs (walked-term (goal->constraints st g) st))
     (define constraints-output
       (cond
         [(null? cxs) "No constraints\n"]
         [else
          (string-append
           "Constraints:\n"
           (string-join
            (map (lambda (v) (~a " * " v)) cxs)
            "\n"))]))
     (string-append qvars-output "\n" constraints-output)]))

(define (serialize-result st qvars)
  (string-append
   "Result:\n"
   (serialize-qvars qvars (walked-term initial-var st))))

(define (serialize-choices choices qvars)
  (define chs (dropf choices state?))
  (define results (takef choices state?))

  (define header
    (cond
      [(and (= 0 (length chs)) (null? results))
       "No more choices available. Undo to continue.\n"]
      [(null? chs) ""]
      [else
       (format "Number of Choices: ~a\n" (length chs))]))

  (define choices-output
    (if (null? chs)
        ""
        (string-join
         (map (lambda (i s)
                (string-append
                 (format "\nChoice ~s:\n" (+ i 1))
                 (serialize-choice s qvars)))
              (range (length chs)) chs)
         "")))

  (define results-output
    (if (null? results)
        ""
        (string-append
         (format "Number of results: ~a\n" (length results))
         (string-join
          (map (lambda (st)
                 (string-append
                  (serialize-result st qvars)
                  "\n"))
               results)
          ""))))

  (string-append header choices-output results-output))

(let ((s (init-explore (query (a b) (appendo a b '(1 2 3 4))))))
  (let ((s (explore-choice s step 0)))
    (display (serialize-choices (explore-node-choices (explore-loc-tree s)) '(a b)))))


(let ((s (init-explore (query (p) (eval-expo p '() p)))))
  (let ((s (explore-choice s step 0)))
    (display (serialize-choices (explore-node-choices (explore-loc-tree s)) '(p)))))


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

(let ((s (init-explore (query (p) (eval-expo p '() p)))))
  (let loop ((s (explore-choice s step 0)))
    ))
(drive/stdio step (query (p) (eval-expo p '() p)))

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

(split-at '(1 2 3 4 5) 2)
