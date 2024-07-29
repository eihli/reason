#lang racket/base
(require zeromq
         first-order-miniKanren/microk-fo
         first-order-miniKanren/tools
         "math.rkt"
         (rename-in "mk-syntax.rkt" (run mk/run))
         "nas.rkt")

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
      (== a `(,a1 . ,a2))
      (== ab `(,a1 . ,res))
      (appendo a2 b res)))))

(define (parse-integer bs)
  (let loop ((r 0)
             (i 0))
    (if (= i 4)
        r
        (loop (+ (arithmetic-shift r 8) (bytes-ref bs i))
              (+ i 1)))))

(define (make-decider socket)
  (lambda (s)
    (let ((out (open-output-string)))
      (write s out)
      (with-output-to-file "/tmp/foo"
        (lambda ()
          (printf "~s\n" s))
        #:exists 'append)
      (zmq-send socket (get-output-string out))
      (let ((response (zmq-recv-string socket)))
        (if (= (parse-integer (string->bytes/utf-8 response)) 0) 's1 's2)))))

(define socket (zmq-socket 'pair #:connect "tcp://127.0.0.5:5555"))

(define (simplify s)
  (dnf/stream s))

(define (mature/step step s)
  (if (mature? s) s (mature/step step (step s))))

(define (stream-take/step step n s)
  (if (eqv? 0 n) '()
      (let ((s (mature/step step s)))
        (if (pair? s)
            (begin
              (with-output-to-file "/tmp/foo"
                (lambda ()
                  (printf "RESULT: ~s\n" (car s)))
                #:exists 'append)
              (cons (car s) (stream-take/step step (and n (- n 1)) (cdr s))))
            '()))))

(define (run socket)
  (let ((s (query (a b) (appendo a b '(1 2 3 4)))))
    (let ((decide (make-decider socket)))
      (map reify/initial-var (stream-take/step (make-nastep decide) 5 s)))))


(define (layer-run socket)
  (let ((s (query (q) (layerso q '(0 0 0 1) '(0 0 1)))))
    (let ((decide (make-decider socket)))
      (map reify/initial-var (stream-take/step (let ((step (make-nastep decide)))
                                                 (lambda (s) (step s))) 10 s)))))

(layer-run socket)

(define decide (make-decider socket))

(define stepper (make-nastep decide))

(let ((s (query (a b) (appendo a b '(1 2 3 4)))))
  (stepper (stepper (stepper s))))
