
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
(struct zip-tre (s) #:prefab)
(struct zip-loc (tree context) #:prefab)
(define zip-TOP 'TOP)
;; Initial zipper: ((zip-tre #f) (zip-ctx -1 '() zip-TOP))

(define (flip i) (if (= i 0) 1 0))

(define (mature? s)
  (or (not s) (pair? s)))

(define (mature z s)
  (if (mature? s)
      (cons z s)
      (let ((result (step/zip z s)))
        (mature (car result) (cdr result)))))

;; --------------------------------------------------
;; Zipper Utilities (for later)
;; --------------------------------------------------
(define (step/zip z s)
  (match s
    ((mplus s1 s2)
     (let* ((ctx (zip-loc-context z))
            (cur-idx (zip-ctx-index ctx))
            (nxt-idx (flip cur-idx)))
       (let* ((s1 (if (= 0 cur-idx)
                      (if (mature? s1) s1 (cdr (step/zip z s1)))
                      s1))
              (s2 (if (= 1 cur-idx)
                      (if (mature? s2) s2 (cdr (step/zip z s2)))
                      s2)))
         (cond
           ((not s1) (cons (zip-loc (zip-tre s2) (zip-ctx nxt-idx `(,s1) (zip-loc-context z))) s2))
           ((not s2) (cons (zip-loc (zip-tre s1) (zip-ctx nxt-idx `(,s2) (zip-loc-context z))) s1))
           ((pair? s1) (cons (zip-loc (zip-tre (cons (car s1) (mplus (cdr s1) s2)))
                                      (zip-ctx cur-idx `(,s2) (zip-loc-context z)))
                             (cons (car s1) (mplus (cdr s1) s2))))
           ((pair? s2) (cons (zip-loc (zip-tre (cons (car s2) (mplus s1 (cdr s2))))
                                      (zip-ctx cur-idx `(,s1) (zip-loc-context z)))
                             (cons (car s2) (mplus s1 (cdr s2)))))
           (else (cons (zip-loc (zip-tre (mplus s1 s2)) (zip-ctx nxt-idx `(,(if (= 0 nxt-idx) s2 s1)) (zip-loc-context z)))
                       (mplus s1 s2)))))))
    ((bind s g)
     (let* ((res (if (mature? s) (cons z s)
                     (step/zip z s)))
            (nxt-z (car res))
            (nxt-s (cdr res)))
       (cond ((not nxt-s) (cons nxt-z #f))
             ((pair? nxt-s)
              (step/zip nxt-z (mplus (pause (car nxt-s) g)
                                     (bind (cdr nxt-s) g))))
             (else (cons nxt-z (bind nxt-s g))))))
    ((pause st g)
     (cons (zip-loc (zip-tre (start st g)) (zip-loc-context z))
           (start st g)))
    (_ (cons z s))))

;; --------------------------------------------------
;; Zipper-Aware Stream Take
;; --------------------------------------------------
(define (stream-take/zip n z s)
  (if (zero? n)
      '()
      (let ((result (mature z s)))
        (let ((new-z (car result))
              (new-s (cdr result)))
          (if (pair? new-s)
              (cons (cons new-z (car new-s))
                    (stream-take/zip (and n (- n 1))
                                    (zip-loc (zip-tre (cdr new-s))
                                            (zip-ctx 0 '() (zip-loc-context new-z)))
                                    (cdr new-s)))
              '())))))


;; --------------------------------------------------
;; API: run/traced
;; --------------------------------------------------

(define-syntax run/zip
  (syntax-rules ()
    ((_ n body ...)
     (stream-take/zip n (zip-loc (query body ...) (zip-ctx 0 '() zip-TOP)) (query body ...)))))

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
      [(eq? ctx zip-TOP) (reverse path)]  ; We've reached the top, reverse for root-to-leaf order
      [else
       (let ([idx (zip-ctx-index ctx)]
             [parent (zip-ctx-parent ctx)])
         (loop parent (cons idx path)))])))  ; Accumulate indices as we go up

(comment

 (length (run/zip 100 (a b) (appendo a b '(1 2 3 4))))
 ;; => 5
 (zipper->path (caar (reverse (run/zip 10 (a b) (appendo a b '(1 2 3 4))))))

 ;; => '(0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 1 0)
 )
