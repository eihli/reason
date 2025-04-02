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
      (let ((s1 (if (mature? s1) s1 (loc-focus (step/zipper (loc '() z '() s1))))))
        (cond ((not s1) (loc `(,s1) z `() s2))
              ((pair? s1)
               (cons (loc `() z `(,s2) (car s1))
                     (loc `(,(cdr s1)) z `(,s2) (mplus (cdr s1) s2))))
              (else (loc `(,s2) z `(,s1) (mplus s2 s1))))))
     ((bind s g)
      (let ((s (if (mature? s) s (loc-focus (step/zipper (loc '() z '() s))))))
        (cond ((not s) #f)
              ((pair? s)
               (step/zipper (loc
                             `(,(pause (car s) g))
                             z
                             `(,(bind (cdr s) g))
                             (mplus (pause (car s) g) (bind (cdr s) g)))))
              (else (loc '() z '() (bind s g))))))
     ((pause st g) (loc '() z '() (start st g)))
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

(caddr (run/traced 3 (a b) (appendo a b '(1 2 3 4))))

(cadr (run/traced 10 (a b) (appendo a b '(1 2 3 4))))

(comment
 (loc
  '(#s(mplus #s(bind #f #s(== #s(var b 185) (1 2 3 4))) #f))
  (loc
   '()
   #f
   '()
   '#s(mplus
       #s(pause
          #s(state ((#s(var #f 0) #s(var a 184) #s(var b 185))) () () ())
          #s(conj
             #s(conj #s(== #s(var a 184) (#s(var a1 186) . #s(var a2 187))) #s(== (1 2 3 4) (#s(var a1 186) . #s(var res 188))))
             #s(relate
                #<procedure:...acket/mk-syntax.rkt:22:15>
                (#<procedure:appendo> appendo #s(var a2 187) #s(var b 185) #s(var res 188)))))
       #s(mplus #s(bind #f #s(== #s(var b 185) (1 2 3 4))) #f)))
  '(#s(mplus
       #s(bind
          #s(mplus #s(bind #f #s(== (1 2 3 4) (#s(var a1 186) . #s(var res 188)))) #f)
          #s(relate #<procedure:...acket/mk-syntax.rkt:22:15> (#<procedure:appendo> appendo #s(var a2 187) #s(var b 185) #s(var res 188))))
       #s(pause
          #s(state
             ((#s(var res 188) 2 3 4)
              (#s(var a1 186) . 1)
              (#s(var a 184) #s(var a1 186) . #s(var a2 187))
              (#s(var #f 0) #s(var a 184) #s(var b 185)))
             ()
             ()
             ())
          #s(disj
             #s(conj #s(== #s(var a2 187) ()) #s(== #s(var b 185) #s(var res 188)))
             #s(conj
                #s(conj #s(== #s(var a2 187) (#s(var a1 189) . #s(var a2 190))) #s(== #s(var res 188) (#s(var a1 189) . #s(var res 191))))
                #s(relate
                   #<procedure:...acket/mk-syntax.rkt:22:15>
                   (#<procedure:appendo> appendo #s(var a2 190) #s(var b 185) #s(var res 191))))))))
  '#s(mplus
      #s(mplus #s(bind #f #s(== #s(var b 185) (1 2 3 4))) #f)
      #s(mplus
         #s(bind
            #s(mplus #s(bind #f #s(== (1 2 3 4) (#s(var a1 186) . #s(var res 188)))) #f)
            #s(relate #<procedure:...acket/mk-syntax.rkt:22:15> (#<procedure:appendo> appendo #s(var a2 187) #s(var b 185) #s(var res 188))))
         #s(pause
            #s(state
               ((#s(var res 188) 2 3 4)
                (#s(var a1 186) . 1)
                (#s(var a 184) #s(var a1 186) . #s(var a2 187))
                (#s(var #f 0) #s(var a 184) #s(var b 185)))
               ()
               ()
               ())
            #s(disj
               #s(conj #s(== #s(var a2 187) ()) #s(== #s(var b 185) #s(var res 188)))
               #s(conj
                  #s(conj #s(== #s(var a2 187) (#s(var a1 189) . #s(var a2 190))) #s(== #s(var res 188) (#s(var a1 189) . #s(var res 191))))
                  #s(relate
                     #<procedure:...acket/mk-syntax.rkt:22:15>
                     (#<procedure:appendo> appendo #s(var a2 190) #s(var b 185) #s(var res 191))))))))))


(let ((solution (cadddr (run/traced 10 (a b) (appendo a b '(1 2 3 4))))))
  (let loop ((path '())
             (solution solution))
    (if (false? solution)
        (reverse path)
        (loop (cons (loc-path solution) path)
              (loc-path solution)))))

(map (lambda (x) (loc-left x)) (run/traced 10 (a b) (appendo a b '(1 2 3 4))))

(map (lambda (x) (loc-right x)) (run/traced 10 (a b) (appendo a b '(1 2 3 4))))

;; (run/traced 5 (fn arg out) (eval-expo `(app ,fn ,arg) '() out))
