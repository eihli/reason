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

(struct loc (left path right focus) #:transparent)

(define (make-initial-loc s)
  (loc '() #f '() s))

;; --------------------------------------------------
;; Zipper Utilities (for later)
;; --------------------------------------------------

(define (loc-up z)
  (match z
    [(loc left (loc pl pp pr pf) right focus)
     (loc (cons focus left) pp pr pf)]
    [_ #f]))

(define (loc-redo z)
  (match z
    [(loc left path (cons r rs) focus)
     (loc (cons focus left) path rs r)]
    [_ #f]))

(define (loc-undo z)
  (match z
    [(loc (cons l ls) path right focus)
     (loc ls path (cons focus right) l)]
    [_ #f]))

;; --------------------------------------------------
;; Zipper-aware maturity
;; --------------------------------------------------

(define (mature? z)
  (let ((s (loc-focus z)))
    (or (not s) (pair? s))))

(define (mature z)
  (if (mature? z)
      z
      (mature (step/zipper z))))

;; --------------------------------------------------
;; Zipper-aware Step Function (Corrected)
;; --------------------------------------------------


(define (step/zipper z)
  (let ((s (loc-focus z)))
    (match s
      ;; === mplus ===
      [(mplus s1 s2)
       (let ((s1 (if (mature? (make-initial-loc s1)) s1 (loc-focus (step/zipper (make-initial-loc s1))))))
         (cond
           ((not s1)
            (loc '() z '() s2)) ;; dead s1

           ((pair? s1)
            ;; ✅ NOTICE: returning a SINGLE loc
            (loc '() z (list s2) (car s1))) ;; biased interleaving, take head

           (else
            (loc (list s2) z (list s1) (mplus s2 s1)))))]
      ;; === bind ===
      [(bind s g)
       (let ((s (if (mature? (make-initial-loc s)) s (loc-focus (step/zipper (make-initial-loc s))))))
         (cond
           ((not s)
            ;; <=== ✅ instead of returning #f, return a loc representing the dead-end
            (loc '() z '() #f))
           ((pair? s)
            (step/zipper (loc (list (pause (car s) g))
                              z
                              (list (bind (cdr s) g))
                              (mplus (pause (car s) g)
                                     (bind (cdr s) g)))))
           (else (loc '() z '() (bind s g)))))]

      ;; === pause ===
      [(pause st g)
       (loc '() z '() (start st g))]

      ;; === mature ===
      [else z])))


;; --------------------------------------------------
;; Zipper-Aware Stream Take
;; --------------------------------------------------

(define (stream-take/traced n z)
  (if (zero? n)
      '()
      (let ((mz (mature z)))
        (let ((res (loc-focus mz)))
          (if (pair? res)
              (cons mz (stream-take/traced (- n 1) (make-initial-loc (cdr res))))
              '())))))

;; --------------------------------------------------
;; API: run/traced
;; --------------------------------------------------

(define-syntax run/traced
  (syntax-rules ()
    ((_ n body ...)
     (stream-take/traced n (make-initial-loc (query body ...))))))

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
           (== a `(,a1 . ,a2))
           (== ab `(,a1 . ,res))
           (appendo a2 b res)))))

(cadr (run/traced 5 (a b) (appendo a b '(1 2 3 4))))

(map (lambda (x) (loc-right x)) (run/traced 10 (a b) (appendo a b '(1 2 3 4))))


(define (path-chosen x)
  (cond
    ((false? x)    #f)
    ((loc-right x) 0)
    ((loc-left x)  1)
    (else          '?)))

(let ((solution (car (run/traced 10 (a b) (appendo a b '(1 2 3 4))))))
  (let loop ((path '())
             (solution solution))
    (if (false? solution)
        (map path-chosen (reverse path))
        (loop (cons (loc-path solution) path)
              (loc-path solution)))))

(comment
(define foo (run/traced 1 (fn arg out) (eval-expo `(app ,fn ,arg) '() out)))

 )

;; (let ((solution (car (run/traced 1 (fn arg out) (eval-expo `(app ,fn ,arg) '() out)))))
;;   (let loop ((path '())
;;              (solution solution))
;;     (if (false? solution)
;;         (map path-chosen (reverse path))
;;         (loop (cons (loc-path solution) path)
;;               (loc-path solution)))))
