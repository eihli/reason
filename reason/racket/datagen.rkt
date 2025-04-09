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
      -1))

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
              (cur-idx (zip-ctx-index-safe ctx))
              (nxt-idx (flip cur-idx)))
         (let* ((s1 (if (= 0 cur-idx)
                        (if (mature? s1) s1 (zip-tre-s-safe
                                             (zip-loc-tree-safe
                                              (step/zip (zip-loc
                                                         (zip-tre s1)
                                                         (zip-loc-context z))))))
                        s1))
                (s2 (if (= 1 cur-idx)
                        (if (mature? s2) s2 (zip-tre-s-safe
                                             (zip-loc-tree-safe
                                              (step/zip (zip-loc
                                                         (zip-tre s2)
                                                         (zip-loc-context z))))))
                        s2)))
           (cond
             ((not s1) (zip-loc (zip-tre s2) (zip-ctx nxt-idx `(,s1) (zip-loc-context z))))
             ((not s2) (zip-loc (zip-tre s1) (zip-ctx nxt-idx `(,s2) (zip-loc-context z))))
             ((pair? s1) (zip-loc (zip-tre (cons (car s1) (mplus (cdr s1) s2)))
                                  (zip-ctx cur-idx `(,s2) (zip-loc-context z))))
             ((pair? s2) (zip-loc (zip-tre (cons (car s2) (mplus s1 (cdr s2))))
                                  (zip-ctx cur-idx `(,s1) (zip-loc-context z))))
             (else (zip-loc (zip-tre (mplus s1 s2))
                            (zip-ctx nxt-idx `(,(if (= 0 nxt-idx) s2 s1)) (zip-loc-context z))))))))
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
;; API: run/traced
;; --------------------------------------------------

(define-syntax run/zip
  (syntax-rules ()
    ((_ n body ...)
     (stream-take/zip n (zip-loc (zip-tre (query body ...)) (top-ctx))))))

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
      [(top-ctx? ctx) (reverse path)]  ; We've reached the top, reverse for root-to-leaf order
      [else
       (let ([idx (zip-ctx-index-safe ctx)]
             [parent (zip-ctx-parent-safe ctx)])
         (loop parent (cons idx path)))])))  ; Accumulate indices as we go up

(comment

 (run/zip 100 (a b) (appendo a b '(1 2 3 4)))

 (map
  (lambda (z)
    (walk* initial-var (state-sub (car (zip-tre-s (zip-loc-tree z))))))
  (run/zip 100 (a b) (appendo a b '(1 2 3 4))))
 ;; => '((() (1 2 3 4)) ((1) (2 3 4)) ((1 2) (3 4)) ((1 2 3) (4)) ((1 2 3 4) ()))

 (zipper->path (car (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 ;; => '(1 0)
 (zipper->path (cadr (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))
 ;; => '(1 0 1 0 1 0)

 ;; => '(0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 1 0)
 (car (run/zip 10 (a b) (appendo a b '(1 2 3 4))))


 (zip-up (cadr (run/zip 10 (a b) (appendo a b '(1 2 3 4)))))

 )
