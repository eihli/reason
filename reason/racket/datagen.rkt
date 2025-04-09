#lang racket

(require (prefix-in rm: racket/match)
         "strings.rkt"
         "utils.rkt"
         "microk-fo.rkt"
         "mk-fo.rkt"
         "tools.rkt")


;; --------------------------------------------------
;; Zipper Structure
;; --------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define explore state to maintain the data held at every step
;; when exploring
;; https://wiki.haskell.org/Zipper
(struct zip-ctx (index siblings parent) #:prefab)
(struct top-ctx () #:prefab)

(define (zip-ctx?-safe ctx)
  (or (zip-ctx? ctx)
      (top-ctx? ctx)))

(define (zip-ctx-index-safe ctx)
  (if (zip-ctx? ctx)
      (zip-ctx-index ctx)
      0))

(define (zip-ctx-siblings-safe ctx)
  (if (zip-ctx? ctx)
      (zip-ctx-siblings ctx)
      #f))

(define (zip-ctx-parent-safe ctx)
  (if (zip-ctx? ctx)
      (zip-ctx-parent ctx)
      #f))

(define (zip-loc-tree-safe loc)
  (if (zip-loc? loc)
      (zip-loc-tree loc)
      #f))

(define (zip-tre-s-safe tre)
  (if (zip-tre? tre)
      (zip-tre-s tre)
      #f))

(struct zip-tre (s) #:prefab)
(struct zip-loc (tree context) #:prefab)
(define (zip-top? z)
  (top-ctx? (zip-loc-context z)))

(define (zip-up z)
  (if (zip-top? z)
      #f
      (let* ((ctx (zip-loc-context z))
             (i (zip-ctx-index-safe ctx))
             (tre (zip-loc-tree-safe z)))
        (zip-loc
         (zip-tre
          (if (= 0 i)
              (mplus (zip-tre-s-safe tre)
                     (car (zip-ctx-siblings-safe ctx)))
              (mplus (car (zip-ctx-siblings-safe ctx))
                     (zip-tre-s-safe tre))))
         (zip-ctx-parent-safe ctx)))))

(comment
 (zip-up (caar (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 )

(define (flip i) (if (= i 0) 1 0))

(define (mature?/stream s)
  (or (not s) (pair? s)))
(define (mature? s)
  (or (not s) (pair? s)))

(define (mature z)
  (if (mature? (zip-tre-s-safe (zip-loc-tree-safe z))) z (mature (step/zip z))))

;; --------------------------------------------------
;; Zipper Utilities (for later)
;; --------------------------------------------------
(define (step/zip z)
  (let ((s (zip-tre-s-safe (zip-loc-tree-safe z))))
    (match s
      ((mplus s1 s2)
       (let* ((ctx (zip-loc-context z))
              (cur-idx (zip-ctx-index-safe ctx)))
         (let* ((s1 (if (= 0 cur-idx)
                        (if (mature? s1) s1 (zip-tre-s-safe
                                             (zip-loc-tree-safe
                                              (step/zip (zip-loc
                                                         (zip-tre s1)
                                                         ctx)))))
                        s1))
                (s2 (if (= 1 cur-idx)
                        (if (mature? s2) s2 (zip-tre-s-safe
                                             (zip-loc-tree-safe
                                              (step/zip (zip-loc
                                                         (zip-tre s2)
                                                         ctx)))))
                        s2)))
           (cond
             ((not s1) (zip-loc (zip-tre s2) (zip-ctx cur-idx `(,s1) ctx)))
             ((not s2) (zip-loc (zip-tre s1) (zip-ctx cur-idx `(,s2) ctx)))
             ((pair? s1) (zip-loc (zip-tre (cons (car s1) (mplus (cdr s1) s2)))
                                  (zip-ctx cur-idx `(,s2) ctx)))
             ((pair? s2) (zip-loc (zip-tre (cons (car s2) (mplus s1 (cdr s2))))
                                  (zip-ctx cur-idx `(,s1) ctx)))
             (else (zip-loc (zip-tre (mplus s1 s2))
                            (zip-ctx (flip cur-idx) `(,(if (= 0 (flip cur-idx)) s2 s1)) ctx)))))))
      ((bind s g)
       (let ((s (if (mature? s)
                    s
                    (zip-tre-s-safe
                     (zip-loc-tree-safe
                      (mature
                       (zip-loc (zip-tre s) (zip-loc-context z))))))))
         (cond ((not s) #f)
               ((pair? s)
                (step/zip (zip-loc (zip-tre (mplus (pause (car s) g)
                                                   (bind (cdr s) g)))
                                   (zip-loc-context z))))
               (else (zip-loc (zip-tre (bind s g))
                              (zip-loc-context z))))))
      ((pause st g)
       (zip-loc (zip-tre (start st g)) (zip-loc-context z)))
      (_ z))))

;; --------------------------------------------------
;; Zipper-Aware Stream Take
;; --------------------------------------------------
(define (stream-take/zip n z)
  (if (zero? n)
      '()
      (let ((z (mature z)))
        (if (pair? (zip-tre-s-safe (zip-loc-tree-safe z)))
            (cons z
                  (stream-take/zip (and n (- n 1))
                                   (zip-loc
                                    (zip-tre (cdr (zip-tre-s-safe (zip-loc-tree-safe z))))
                                    (zip-loc-context z))))
            '()))))


;; --------------------------------------------------
;; API: run/zip
;; --------------------------------------------------

(define-syntax run/zip
  (syntax-rules ()
    ((_ n body ...)
     (stream-take/zip n (zip-loc (zip-tre (query body ...)) (top-ctx))))))

;; --------------------------------------------------
;; API: run/zip/path
;; --------------------------------------------------
(define-syntax run/zip/path
  (syntax-rules ()
    ((_ path body ...)
     (let ((z (zip-loc (zip-tre (query body ...)) (top-ctx))))
       (step/zip/path z path)))))

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
           (== a `(,a1 . ,a2))
           (== ab `(,a1 . ,res))
           (appendo a2 b res)))))

;; Traverse up a zipper and collect the path of choices (0 for s1, 1 for s2)
;; Returns a list of indices representing the path from root to current location
(define (zipper->path z)
  (let loop ([ctx (zip-loc-context z)]
             [path '()])
    (cond
      [(top-ctx? ctx) path]  ; We've reached the top, reverse for root-to-leaf order
      [else
       (let ([idx (zip-ctx-index-safe ctx)]
             [parent (zip-ctx-parent-safe ctx)])
         (loop parent (cons idx path)))])))  ; Accumulate indices as we go up

(comment

 (run/zip 1 (a b) (appendo a b '(1 2 3 4)))

 (map
  (lambda (z)
    (walk* initial-var (state-sub (car (zip-tre-s (zip-loc-tree z))))))
  (run/zip 100 (a b) (appendo a b '(1 2 3 4))))
 ;; => '((() (1 2 3 4)) ((1) (2 3 4)) ((1 2) (3 4)) ((1 2 3) (4)) ((1 2 3 4) ()))

 (zipper->path (car (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 ;; => '(0 1)
 (zipper->path (cadr (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 ;; => '(0 1 0 1 0 1)

 ;; => '(0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 1 0)
 (car (run/zip 10 (a b) (appendo a b '(1 2 3 4))))

 ;; Demo of run/zip/path
 ;; First, let's get a path from a regular run/zip
 (define path-example (zipper->path (car (run/zip 10 (a b) (appendo a b '(1 2 3 4))))))
 path-example
 ;; => '(1)

 ;; Now use that path with run/zip/path to navigate directly to the same position
 (define z1 (run/zip/path path-example (a b) (appendo a b '(1 2 3 4))))
 z1
 (walk* initial-var (state-sub (zip-tre-s (zip-loc-tree z1))))

 ;; Verify we got to the same position
 (walk* initial-var (state-sub (car (zip-tre-s (zip-loc-tree z1)))))
 ;; => '(() (1 2 3 4))
 
 ;; We can also try a different path
 (define z2 (run/zip/path '(1 0 1 0 1 0) (a b) (appendo a b '(1 2 3 4))))
 (walk* initial-var (state-sub (car (zip-tre-s (zip-loc-tree z2)))))
 ;; => '((1 2) (3 4))
 
 ;; This demonstrates that run/zip/path allows us to navigate directly to specific positions
 ;; in the search space without having to explore all the intermediate positions.

 (zip-up (cadr (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 )


(define (step/zip/path z p)
  ;; Given a path that looks like this, for example:
  ;; '(1 0 1 0 1 1 0)
  ;; step through the zipper, taking the given path.
  ;; The regular `step/zip` alternates taking the 0 or the 1 path
  ;; in an `mplus`. But this one takes the path given by the user.
  (if (null? p)
      z  ; If path is empty, we've reached the destination
      (let ((s (zip-tre-s-safe (zip-loc-tree-safe z))))
        (match s
          ((mplus s1 s2)
           (let* ((path-choice (car p))  ; The next choice in our path
                  (remaining-path (cdr p)))  ; The rest of the path
             (let* ((s1 (if (= 0 path-choice)
                            (if (mature? s1) s1 (zip-tre-s-safe
                                                 (zip-loc-tree-safe
                                                  (step/zip/path (zip-loc
                                                                  (zip-tre s1)
                                                                  (zip-loc-context z))
                                                                 remaining-path))))
                            s1))
                    (s2 (if (= 1 path-choice)
                            (if (mature? s2) s2 (zip-tre-s-safe
                                                 (zip-loc-tree-safe
                                                  (step/zip/path (zip-loc
                                                                  (zip-tre s2)
                                                                  (zip-loc-context z))
                                                                 remaining-path))))
                            s2)))
               (cond
                 ((not s1) (zip-loc (zip-tre s2) (zip-ctx path-choice `(,s1) (zip-loc-context z))))
                 ((not s2) (zip-loc (zip-tre s1) (zip-ctx path-choice `(,s2) (zip-loc-context z))))
                 ((pair? s1) (zip-loc (zip-tre (cons (car s1) (mplus (cdr s1) s2)))
                                      (zip-ctx path-choice `(,s2) (zip-loc-context z))))
                 ((pair? s2) (zip-loc (zip-tre (cons (car s2) (mplus s1 (cdr s2))))
                                      (zip-ctx path-choice `(,s1) (zip-loc-context z))))
                 (else (zip-loc (zip-tre (mplus s1 s2))
                                (zip-ctx path-choice `(,(if (= 0 path-choice) s2 s1)) (zip-loc-context z))))))))
          ((bind s g)
           (let ((s (if (mature? s)
                        s
                        (zip-tre-s-safe
                         (zip-loc-tree-safe
                          (mature
                           (zip-loc (zip-tre s) (zip-loc-context z))))))))
             (cond ((not s) #f)
                   ((pair? s)
                    (step/zip/path (zip-loc (zip-tre (mplus (pause (car s) g)
                                                            (bind (cdr s) g)))
                                            (zip-loc-context z))
                                   p))
                   (else (zip-loc (zip-tre (bind s g))
                                  (zip-loc-context z))))))
          ((pause st g)
           (zip-loc (zip-tre (start st g)) (zip-loc-context z)))
          (_ z)))))
