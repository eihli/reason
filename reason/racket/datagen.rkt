#lang racket

(require (prefix-in rm: racket/match)
         "strings.rkt"
         "utils.rkt"
         "microk-fo.rkt"
         "mk-fo.rkt"
         "tools.rkt")

;; This is your search-tree zipper structure
(struct loc (left path right focus) #:transparent)

;; Go up one level
(define (loc-up z)
  (rm:match z
    [(loc left (loc pl pp pr pf) right focus)
     (loc (cons focus left) pp pr pf)]
    [_ #f]))

;; Go to the next sibling (redo)
(define (loc-redo z)
  (rm:match z
    [(loc left path (cons r rs) focus)
     (loc (cons focus left) path rs r)]
    [_ #f]))

;; Undo (back to previous sibling)
(define (loc-undo z)
  (match z
    [(loc (cons l ls) path right focus)
     (loc ls path (cons focus right) l)]
    [_ #f]))

;; Reconstruct the path from root to current focus
(define (reconstruct-loc-path z)
  (if (loc? z)
      (cons (loc-focus z) (if (loc-path (loc-path z)) (loc-path (loc-path z)) '()))
      '()))

;; Zipper-aware step
(define (step/zipper z)
  (let ((s (loc-focus z)))
    (rm:match
     s
     ((mplus s1 s2)
      (let ((s1 (if (mature? s1) s1 (loc-focus (step/zipper (loc '() (loc-path z) '() s1))))))
        (cond ((not s1) (loc '() (loc-path z) '() s2))
              ((pair? s1)
               (cons (loc '() z `(,(mplus s2 (cdr s1))) (car s1)) (loc `(,(car s1)) z '() (mplus s2 (cdr s1)))))
              (else (loc `(,s1) z '() (mplus s2 s1))))))
     ((bind s g)
      (let ((s (if (mature? s) s (loc-focus (step/zipper (loc '() (loc-path z) '() s))))))
        (cond ((not s) #f)
              ((pair? s)
               (step/zipper (loc '() (loc-path z) '() (mplus (pause (car s) g)
                                                             (bind (cdr s) g)))))
              (else (loc '() (loc-path z) '() (bind s g))))))
     ((pause st g) (loc '() (loc-path z) '() (start st g)))
     (_ z))))

;; Take n solutions with their final zipper
(define (stream-take/traced n s)
  (if (zero? n)
      '()
      (let* ((z (step/zipper (make-initial-loc s)))
             (res (mature (loc-focus z))))
        (if (pair? res)
            (cons z (stream-take/traced (- n 1) (cdr res)))
            '()))))

(define (make-initial-loc s)
  (loc '() #f '() s))

(define-syntax run/traced
  (syntax-rules ()
    ((_ n body ...)
     (stream-take/traced n (query body ...)))))

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
           (== a `(,a1 . ,a2))
           (== ab `(,a1 . ,res))
           (appendo a2 b res)))))

(walk* initial-var (state-sub (car (mature (loc-focus (cadr (run/traced 2 (a b) (appendo a b '(1 2 3 4)))))))))
(walk* initial-var (state-sub (car (mature (loc-focus (caddr (run/traced 3 (a b) (appendo a b '(1 2 3 4)))))))))


;; (run/traced 5 (fn arg out) (eval-expo `(app ,fn ,arg) '() out))
